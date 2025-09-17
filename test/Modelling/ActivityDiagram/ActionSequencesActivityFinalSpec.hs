module Modelling.ActivityDiagram.ActionSequencesActivityFinalSpec where

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode (..),
  AdConnection (..)
  )

import Test.Hspec(Spec, context, describe, it, shouldBe)

spec :: Spec
spec =
  describe "ActionSequences with Activity Final nodes" $ do
    context "simple diagram with Activity Final" $ do
      it "generates a valid sequence ending at Activity Final" $
        generateActionSequence testDiagramSimpleActivityFinal `shouldBe` ["A"]
      it "validates a sequence ending at Activity Final" $
        validActionSequence ["A"] testDiagramSimpleActivityFinal `shouldBe` True
    context "fork diagram with Activity Final" $ do
      it "validates sequence ['A','B'] that reaches Activity Final and terminates all flows" $
        validActionSequence ["A","B"] testDiagramForkActivityFinal `shouldBe` True
      it "rejects incomplete sequence ['A'] that doesn't reach termination" $
        validActionSequence ["A"] testDiagramForkActivityFinal `shouldBe` False  
      it "validates sequence ['A','C','B'] where Activity Final terminates all flows" $
        validActionSequence ["A","C","B"] testDiagramForkActivityFinal `shouldBe` True

-- Simple diagram: Initial -> A -> Activity Final
testDiagramSimpleActivityFinal :: UMLActivityDiagram
testDiagramSimpleActivityFinal = UMLActivityDiagram
  { nodes =
    [ AdInitialNode { label = 1 }
    , AdActionNode { label = 2, name = "A" }
    , AdActivityFinalNode { label = 3 }
    ]
  , connections =
    [ AdConnection { from = 1, to = 2, guard = "" }
    , AdConnection { from = 2, to = 3, guard = "" }
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
