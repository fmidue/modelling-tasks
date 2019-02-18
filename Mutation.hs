module Mutation where

import Types
import Edges

import Data.Function (on)
import Data.List     ((\\))
import Data.Set      (Set, fromList, toList)

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

{-|
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

allIncreaseLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allIncreaseLimitsRange = allLimitsWith ((>) `on` limitSize)

allDecreaseLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allDecreaseLimitsRange = allLimitsWith ((<) `on` limitSize)

allShiftDownLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftDownLimitsRange =
  allLimitsWith (\x y -> limitSize x == limitSize y && fst x > fst y)

allShiftUpLimitsRange :: Targets -> [DiagramEdge] -> [[DiagramEdge]]
allShiftUpLimitsRange =
  allLimitsWith (\x y -> limitSize x == limitSize y && fst x > fst y)

{-|
Returns all possible sets of edges by modifying the limits on one side of one
edge by the given modification op on applying targets.
-}
allLimitsWith :: (Limit -> Limit -> Bool) -> Targets -> [DiagramEdge] -> [[DiagramEdge]]
allLimitsWith op ts es =
  [(sv, ev, Assoc k sl'  el' False):filter (e /=) es
  | e@(sv, ev, Assoc k sl el _) <- es, t <- toList ts, isTargetEdge e t
  , (sl', el') <- bothLimits sl el t]
  where
    bothLimits s e t = zip (repeat s) (endLimits e t)
                    ++ zip (startLimits s t) (repeat e)
    startLimits l t = [l' | l' <- fst $ allLimits t, l' `op` l]
    endLimits   l t = [l' | l' <- snd $ allLimits t, l' `op` l]

{-|
Beware! This function just takes a constant value (at the moment 10) to measure
the size of unlimited upper bounds.
-}
limitSize :: Limit -> Int
limitSize (x, Nothing) = 10 - x
limitSize (x, Just y ) = y - x
