module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (
  SelectASConfig(..),
  SelectASSolution(correctSequence),
  checkSelectASConfig,
  defaultSelectASConfig,
  selectActionSequence
  )

import Modelling.ActivityDiagram.ActionSequences (actionRepetitionDistance)
import Modelling.ActivityDiagram.Config (
  AdConfig (objectNodeLimits, cycles),
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  AdNode(..),
  AdConnection(..)
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "checkSelectASConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectASConfig defaultSelectASConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $ do
      it "it returns a String with necessary changes" $
        checkSelectASConfig defaultSelectASConfig {
          adConfig = defaultAdConfig {objectNodeLimits = (0, 1)},
          objectNodeOnEveryPath = Just True
        } `shouldSatisfy` isJust
      it "rejects withActionRepetition when cycles = 0" $
        checkSelectASConfig defaultSelectASConfig {
          withActionRepetition = True,
          adConfig = defaultAdConfig {cycles = 0}
        } `shouldSatisfy` isJust
      it "accepts withActionRepetition = True when cycles >= 1" $
        checkSelectASConfig defaultSelectASConfig {
          withActionRepetition = True,
          adConfig = defaultAdConfig {cycles = 1}
        } `shouldBe` Nothing

  describe "selectActionSequence with repetition" $ do
    let testDiagram = UMLActivityDiagram {
          nodes = [
            AdInitialNode {label = 1},
            AdActionNode {label = 2, name = "A"},
            AdForkNode {label = 3},
            AdActionNode {label = 4, name = "B"},
            AdActionNode {label = 5, name = "C"},
            AdMergeNode {label = 6},
            AdActionNode {label = 7, name = "D"},
            AdActionNode {label = 8, name = "E"},
            AdDecisionNode {label = 9},
            AdJoinNode {label = 10},
            AdActionNode {label = 11, name = "F"},
            AdFlowFinalNode {label = 12},
            AdMergeNode {label = 13},
            AdDecisionNode  {label = 14}
          ],
          connections = [
            AdConnection {from = 1, to = 2, guard = ""}, -- Initial node to A
            AdConnection {from = 2, to = 3, guard = ""}, -- A to fork
            AdConnection {from = 3, to = 13, guard = ""}, -- left fork path (directly to merge)
            AdConnection {from = 3, to = 5, guard = ""}, -- right fork path (C)
            AdConnection {from = 5, to = 6, guard = ""}, -- C to merge,
            AdConnection {from = 6, to = 7, guard = ""}, -- merge to D
            AdConnection {from = 7, to = 8, guard = ""}, -- D to E
            AdConnection {from = 8, to = 9, guard = ""}, -- E to decision
            AdConnection {from = 9, to = 6, guard = ""}, -- decision to merge
            AdConnection {from = 9, to = 10, guard = ""}, -- decision to join
            AdConnection {from = 10, to = 11, guard = ""}, -- join to F
            AdConnection {from = 11, to = 12, guard = ""}, -- F to flow final node,
            AdConnection {from = 13, to = 4, guard = ""}, -- left fork path (merge to B)
            AdConnection {from = 4, to = 14, guard = ""}, -- B to decision (left fork path)
            AdConnection {from = 14, to = 13, guard = ""}, -- decision to merge (left fork path)
            AdConnection {from = 14, to = 10, guard = ""} -- decision to join (left fork path)
          ]
        }
    context "when withActionRepetition = False" $
      it "generates sequences without forcing repetition" $ do
        let Just solution = selectActionSequence False 2 testDiagram
            actionSeq = correctSequence solution
        length actionSeq `shouldSatisfy` (>= 2)
    context "when withActionRepetition = True" $
      it "generates sequences with repetition" $ do
        let Just solution = selectActionSequence True 2 testDiagram
            actionSeq = correctSequence solution
        actionRepetitionDistance actionSeq `shouldSatisfy` maybe False (>= 1)
