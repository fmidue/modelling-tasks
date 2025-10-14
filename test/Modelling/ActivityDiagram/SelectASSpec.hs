module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (
  SelectASConfig(..),
  SelectASSolution(correctSequence),
  checkSelectASConfig,
  defaultSelectASConfig,
  selectActionSequence
  )

import Modelling.ActivityDiagram.ActionSequences (hasActionRepetitionWithMinDistance)
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
      it "rejects minActionRepetitionDistance >= 0 when cycles = 0" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = 0,
          adConfig = defaultAdConfig {cycles = 0}
        } `shouldSatisfy` isJust
      it "rejects minActionRepetitionDistance < -1" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = -2
        } `shouldSatisfy` isJust
      it "accepts minActionRepetitionDistance >= 0 when cycles >= 1" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = 1,
          adConfig = defaultAdConfig {cycles = 1}
        } `shouldBe` Nothing

  describe "selectActionSequence with repetition" $ do
    let testDiagram = UMLActivityDiagram {
          nodes = [
            AdActionNode {label = 1, name = "A"},
            AdActionNode {label = 2, name = "B"},
            AdActionNode {label = 3, name = "C"},
            AdInitialNode {label = 4},
            AdFlowFinalNode {label = 5},
            AdDecisionNode {label = 6},
            AdMergeNode {label = 7}
          ],
          connections = [
            AdConnection {from = 4, to = 1, guard = ""},
            AdConnection {from = 1, to = 7, guard = ""},
            AdConnection {from = 7, to = 2, guard = ""},
            AdConnection {from = 2, to = 6, guard = ""},
            AdConnection {from = 6, to = 5, guard = "end"},
            AdConnection {from = 6, to = 3, guard = "loop"},
            AdConnection {from = 3, to = 7, guard = ""}
          ]
        }
    context "when minActionRepetitionDistance = -1" $
      it "generates sequences without forcing repetition" $ do
        let solution = selectActionSequence (-1) 2 testDiagram
            actionSeq = correctSequence solution
        length actionSeq `shouldSatisfy` (>= 2)
    context "when minActionRepetitionDistance = 0" $
      it "generates sequences with immediate repetition when possible" $ do
        let solution = selectActionSequence 0 2 testDiagram
            actionSeq = correctSequence solution
        hasActionRepetitionWithMinDistance 0 actionSeq `shouldBe` True
    context "when minActionRepetitionDistance = 1" $
      it "generates sequences with at least 1 action between repetitions" $ do
        let solution = selectActionSequence 1 2 testDiagram
            actionSeq = correctSequence solution
        hasActionRepetitionWithMinDistance 1 actionSeq `shouldBe` True
