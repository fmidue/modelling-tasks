module Modelling.ActivityDiagram.ActionSequencesActivityFinalSpec where

import Modelling.ActivityDiagram.ActionSequences (validActionSequence)

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..)
  )

import Test.Hspec(Spec, context, describe, it, shouldBe)

spec :: Spec
spec =
  describe "ActionSequences with Activity Final nodes" $ do
    context "fork diagram with Activity Final" $ do
      it "validates sequence ['A','B'] that reaches Activity Final and terminates all flows" $
        validActionSequence ["A","B"] testDiagramForkActivityFinal `shouldBe` True
      it "rejects incomplete sequence ['A'] that doesn't reach termination" $
        validActionSequence ["A"] testDiagramForkActivityFinal `shouldBe` False
      it "validates sequence ['A','C','B'] where Activity Final terminates all flows" $
        validActionSequence ["A","C","B"] testDiagramForkActivityFinal `shouldBe` True
    context "fork diagram with Activity Final through Object nodes" $ do
      it "validates sequence ['A','B'] that reaches Activity Final through Object node" $
        validActionSequence ["A","B"] testDiagramForkActivityFinalWithObjects `shouldBe` True
      it "rejects sequence ['A','C'] that only reaches Flow Final through Object node" $
        validActionSequence ["A","C"] testDiagramForkActivityFinalWithObjects `shouldBe` False
      it "validates sequence ['A','C','B'] where Activity Final terminates all flows after Flow Final" $
        validActionSequence ["A","C","B"] testDiagramForkActivityFinalWithObjects `shouldBe` True

-- Fork diagram with Activity Final through Object nodes
testDiagramForkActivityFinalWithObjects :: UMLActivityDiagram
testDiagramForkActivityFinalWithObjects = UMLActivityDiagram
  { nodes =
    [ AdInitialNode { label = 1 }
    , AdActionNode { label = 2, name = "A" }
    , AdForkNode { label = 3 }
    , AdActionNode { label = 4, name = "B" }
    , AdActionNode { label = 5, name = "C" }
    , AdObjectNode { label = 6, name = "ObjB" }  -- Object node before Activity Final
    , AdObjectNode { label = 7, name = "ObjC" }  -- Object node before Flow Final
    , AdActivityFinalNode { label = 8 }
    , AdFlowFinalNode { label = 9 }
    ]
  , connections =
    [ AdConnection { from = 1, to = 2, guard = "" }
    , AdConnection { from = 2, to = 3, guard = "" }
    , AdConnection { from = 3, to = 4, guard = "" }  -- Fork -> B
    , AdConnection { from = 3, to = 5, guard = "" }  -- Fork -> C
    , AdConnection { from = 4, to = 6, guard = "" }  -- B -> ObjB
    , AdConnection { from = 5, to = 7, guard = "" }  -- C -> ObjC
    , AdConnection { from = 6, to = 8, guard = "" }  -- ObjB -> Activity Final
    , AdConnection { from = 7, to = 9, guard = "" }  -- ObjC -> Flow Final
    ]
  }

-- Fork diagram: Initial -> A -> Fork -> (B -> Activity Final, C -> Flow Final)
testDiagramForkActivityFinal :: UMLActivityDiagram
testDiagramForkActivityFinal = UMLActivityDiagram
  { nodes =
    [ AdInitialNode { label = 1 }
    , AdActionNode { label = 2, name = "A" }
    , AdForkNode { label = 3 }
    , AdActionNode { label = 4, name = "B" }
    , AdActionNode { label = 5, name = "C" }
    , AdActivityFinalNode { label = 6 }
    , AdFlowFinalNode { label = 7 }
    ]
  , connections =
    [ AdConnection { from = 1, to = 2, guard = "" }
    , AdConnection { from = 2, to = 3, guard = "" }
    , AdConnection { from = 3, to = 4, guard = "" }
    , AdConnection { from = 3, to = 5, guard = "" }
    , AdConnection { from = 4, to = 6, guard = "" }  -- B leads to Activity Final
    , AdConnection { from = 5, to = 7, guard = "" }  -- C leads to Flow Final
    ]
  }
