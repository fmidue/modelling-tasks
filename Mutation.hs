module Mutation where

import Types
import Edges

data Mutation =
    Add Addition
  | Remove Removal
  | ChangeMultiplicity
  | Transform Transformation

data Addition       = AddInheritance     | AddComposition    | AddOther
data Removal        = RemoveInheritance  | RemoveComposition | RemoveOther
data Transformation = FromInheritance    | ToInheritance
                    | CompositionToOther | OtherToComposition
                    | OtherToOther

allInheritances :: [DiagramEdge] -> [DiagramEdge]
allInheritances xs =
  [x | x@(_, _, Inheritance) <- xs]

allAssocs :: [DiagramEdge] -> [DiagramEdge]
allAssocs xs =
  [x | x@(_, _, Assoc {}) <- xs]

allCompositions :: [DiagramEdge] -> [DiagramEdge]
allCompositions xs =
  [x | x@(_, _, Assoc Composition _ _ _) <- xs]

allNonCompositions :: [DiagramEdge] -> [DiagramEdge]
allNonCompositions xs =
  [x | x@(_, _, k) <- xs, not $ isComposition k]

allOthers :: [DiagramEdge] -> [DiagramEdge]
allOthers xs =
  [x | x@(_, _, Assoc t _ _ _) <- xs, t /= Composition]

allRemoves :: Removal -> [DiagramEdge] -> [[DiagramEdge]]
allRemoves r es =
  let xs = case r of
             RemoveInheritance -> allInheritances es
             RemoveComposition -> allCompositions es
             RemoveOther       -> allOthers es
  in [filter (x /=) es | x <- xs]

