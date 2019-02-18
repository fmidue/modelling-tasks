module Mutation where

import Types
import Edges

import Data.List ((\\))
import Data.Set  (Set, fromList, toList)

data Target = TAssociation | TAggregation | TComposition | TInheritance
  deriving (Bounded, Enum, Eq, Ord)

type Targets = Set Target

data Mutation =
    Add       Target
  | Remove    Target
  | Transform Target Target
  | MultiplicityRange Alteration Target

data Alteration = Increase | Decrease

targetSet :: Targets
targetSet = fromList [minBound ..]

isTarget :: Connection -> Target -> Bool
isTarget (Assoc Association _ _ _) TAssociation = True
isTarget (Assoc Aggregation _ _ _) TAggregation = True
isTarget (Assoc Composition _ _ _) TComposition = True
isTarget Inheritance               TInheritance = True
isTarget _                         _            = False

isTargetEdge :: DiagramEdge -> Target -> Bool
isTargetEdge (_, _, t) = isTarget t

isTargetsEdge :: DiagramEdge -> Targets -> Bool
isTargetsEdge x ts = any (x `isTargetEdge`) ts

targets :: Targets -> [DiagramEdge] -> [DiagramEdge]
targets ts es =
  [e | e <- es, isTargetsEdge e ts]

nonTargets :: Targets -> [DiagramEdge] -> [DiagramEdge]
nonTargets ts es =
  [e | e <- es, not $ isTargetsEdge e ts]

allRemoves :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allRemoves ts es =
  [filter (e /=) es | e <- es, isTargetsEdge e ts]

nonEdges :: [String] -> [DiagramEdge] -> [(String, String)]
nonEdges vs es = [(x, y) | x <- vs, y <- vs, x < y] \\ connections
  where
    connections = [e | (x, y, _) <- es, e <- [(x, y), (y, x)]]

type Limit = (Int, Maybe Int)

allAdds :: Targets -> [String] -> [DiagramEdge] -> [[DiagramEdge]]
allAdds ts vs es =
  [x:es | (s, e) <- nonEdges vs es, t <- toList ts
        , sl <- fst $ allLimits t, el <- snd $ allLimits t
        , x <- addEdges s e t sl el]
  where
    addEdges s e TInheritance _  _  = [(s, e, Inheritance), (e, s, Inheritance)]
    addEdges s e TAssociation sl el = [addEdge s e TAssociation sl el]
    addEdges s e t            sl el = [addEdge s e t sl el, addEdge e s t sl el]
    addEdge s e t sl el = (s, e, Assoc (assocType t) sl el False)
    assocType TAssociation = Association
    assocType TAggregation = Aggregation
    assocType TComposition = Composition
    assocType TInheritance = error "An inheritance is no Assoc"

{- |
Generates a list of all limits (i.e. multiplicities) for the given target.
The resulting tuple contains the list of all multiplicities at the edges start
and the list of all multiplicities at the edges end.
-}
allLimits :: Target -> ([Limit], [Limit])
allLimits t = (allStartLimits, allEndLimits)
  where
    allStartLimits = case t of
      TInheritance -> []
      TComposition -> [(0, Just 1), (1, Just 1)]
      _            -> allPossibleLimits
    allEndLimits   = case t of
      TInheritance -> []
      _            -> allPossibleLimits
    allPossibleLimits = [(l, h) | l <- [0, 1, 2], h <- [Just 1, Just 2, Nothing]
                                , maybe True (l <=) h]
