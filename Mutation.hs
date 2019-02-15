module Mutation where

import Types
import Edges

import Data.List ((\\))
import Data.Set  (Set, fromList)

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
