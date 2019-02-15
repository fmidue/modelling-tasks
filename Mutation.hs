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

allRemoves :: Targets -> [DiagramEdge] -> [DiagramEdge]
allRemoves ts es =
  [e | e <- es, isTargetsEdge e ts]

applyRemove :: [DiagramEdge] -> DiagramEdge -> [DiagramEdge]
applyRemove es e = filter (e /=) es

nonEdges :: [String] -> [DiagramEdge] -> [(String, String)]
nonEdges vs es = [(x, y) | x <- vs, y <- vs, x < y] \\ connections
  where
    connections = [e | (x, y, _) <- es, e <- [(x, y), (y, x)]]

type AddEdge = Either (Target, Multiplicity -> Multiplicity -> DiagramEdge) DiagramEdge
type Multiplicity = (Int, Maybe Int)

allAdds :: Targets -> [String] -> [DiagramEdge] -> [AddEdge]
allAdds ts vs es =
  [x | (s, e) <- nonEdges vs es, t <- toList ts, x <- addEdges s e t]
  where
    addEdges s e TInheritance = [Right (s, e, Inheritance), Right (e, s, Inheritance)]
    addEdges s e t            =
      let addEdge s' e' = Left (t, \x y -> (s', e', Assoc (assocType t) x y False))
      in case t of
        TAssociation -> [addEdge s e]
        _            -> [addEdge s e, addEdge e s]
    assocType TAssociation = Association
    assocType TAggregation = Aggregation
    assocType TComposition = Composition
    assocType TInheritance = error "An inheritance is no Assoc"

applyAdd :: Functor f => [DiagramEdge] -> (AddEdge -> f DiagramEdge) -> AddEdge -> f [DiagramEdge]
applyAdd es f e = (:es) <$> f e
