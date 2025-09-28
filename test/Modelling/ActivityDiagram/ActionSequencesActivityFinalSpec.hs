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
  describe "Activity Final node behavior" $ do
    context "simple linear sequence with Activity Final" $ do
      it "generates a correct sequence ending with Activity Final" $
        generateActionSequence simpleActivityFinalDiagram `shouldBe` ["A", "B"]
      it "accepts sequence that reaches Activity Final" $
        validActionSequence ["A", "B"] simpleActivityFinalDiagram `shouldBe` True
      it "rejects incomplete sequence not reaching Activity Final" $
        validActionSequence ["A"] simpleActivityFinalDiagram `shouldBe` False

    context "fork with Activity Final on one branch" $ do
      it "generates sequence leading to Activity Final (terminates all flows)" $
        generateActionSequence forkWithActivityFinalDiagram `shouldBe` ["A", "B"]
      it "accepts sequence that reaches Activity Final and terminates all flows" $
        validActionSequence ["A", "B"] forkWithActivityFinalDiagram `shouldBe` True
      it "rejects sequence going through Flow Final branch (doesn't terminate all flows)" $
        validActionSequence ["A", "C"] forkWithActivityFinalDiagram `shouldBe` False
      it "rejects sequence executing both branches when Activity Final should terminate all" $
        validActionSequence ["A", "B", "C"] forkWithActivityFinalDiagram `shouldBe` False
      it "accepts sequence executing C then B (Activity Final terminates all flows even if one branch already terminated)" $
        validActionSequence ["A", "C", "B"] forkWithActivityFinalDiagram `shouldBe` True

    context "fork with Flow Final on one branch" $ do
      it "generates some sequence that terminates all flows properly" $
        let generated = generateActionSequence forkWithFlowFinalDiagram
        in validActionSequence generated forkWithFlowFinalDiagram `shouldBe` True
      it "accepts sequence that executes both branches when only Flow Final present" $
        validActionSequence ["A", "B", "C"] forkWithFlowFinalDiagram `shouldBe` True
      it "accepts sequence that executes both branches in different order" $
        validActionSequence ["A", "C", "B"] forkWithFlowFinalDiagram `shouldBe` True
      it "rejects sequence that doesn't execute both branches" $
        validActionSequence ["A", "B"] forkWithFlowFinalDiagram `shouldBe` False
      it "rejects sequence that doesn't execute both branches (other branch)" $
        validActionSequence ["A", "C"] forkWithFlowFinalDiagram `shouldBe` False

-- Simple diagram: Initial -> A -> B -> Activity Final
simpleActivityFinalDiagram :: UMLActivityDiagram
simpleActivityFinalDiagram = UMLActivityDiagram
  { nodes =
    [ AdInitialNode { label = 1 }
    , AdActionNode { label = 2, name = "A" }
    , AdActionNode { label = 3, name = "B" }
    , AdActivityFinalNode { label = 4 }
    ]
  , connections =
    [ AdConnection { from = 1, to = 2, guard = "" }
    , AdConnection { from = 2, to = 3, guard = "" }
    , AdConnection { from = 3, to = 4, guard = "" }
    ]
  }

-- Fork diagram: Initial -> A -> Fork -> (B -> Activity Final, C -> Flow Final)
forkWithActivityFinalDiagram :: UMLActivityDiagram
forkWithActivityFinalDiagram = UMLActivityDiagram
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
    , AdConnection { from = 4, to = 6, guard = "" }  -- B -> Activity Final
    , AdConnection { from = 5, to = 7, guard = "" }  -- C -> Flow Final
    ]
  }

-- Fork diagram: Initial -> A -> Fork -> (B -> Flow Final, C -> Flow Final)
forkWithFlowFinalDiagram :: UMLActivityDiagram
forkWithFlowFinalDiagram = UMLActivityDiagram
  { nodes =
    [ AdInitialNode { label = 1 }
    , AdActionNode { label = 2, name = "A" }
    , AdForkNode { label = 3 }
    , AdActionNode { label = 4, name = "B" }
    , AdActionNode { label = 5, name = "C" }
    , AdFlowFinalNode { label = 6 }
    , AdFlowFinalNode { label = 7 }
    ]
  , connections =
    [ AdConnection { from = 1, to = 2, guard = "" }
    , AdConnection { from = 2, to = 3, guard = "" }
    , AdConnection { from = 3, to = 4, guard = "" }
    , AdConnection { from = 3, to = 5, guard = "" }
    , AdConnection { from = 4, to = 6, guard = "" }  -- B -> Flow Final
    , AdConnection { from = 5, to = 7, guard = "" }  -- C -> Flow Final
    ]
  }
