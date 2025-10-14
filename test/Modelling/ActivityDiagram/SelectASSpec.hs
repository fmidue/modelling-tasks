module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (
  SelectASConfig(..),
  SelectASSolution(correctSequence),
  checkSelectASConfig,
  defaultSelectASConfig,
  selectActionSequence
  )

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

-- | Check if a sequence of action names has repetition with at least the specified minimum distance
-- between repeated actions. For example:
-- minDistance = 0: [A,A,...] is valid (immediate repetition)
-- minDistance = 1: [A,B,A,...] is valid (at least 1 action between)
-- minDistance = 2: [A,B,C,A,...] is valid (at least 2 actions between)
hasActionRepetitionWithMinDistance :: Int -> [String] -> Bool
hasActionRepetitionWithMinDistance minDistance actionSequence =
  let -- Find all pairs of indices where the same action occurs
      indicesOf action = [i | (i, a) <- zip [0..] actionSequence, a == action]
      -- Check if any action has two occurrences with sufficient distance
      checkAction action =
        let indices = indicesOf action
        in any (\(i, j) -> j - i - 1 >= minDistance) [(i, j) | i <- indices, j <- indices, i < j]
  in any checkAction $ nub actionSequence
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

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
      it "rejects minActionRepetitionDistance when cycles = 0" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = Just 0,
          adConfig = defaultAdConfig {cycles = 0}
        } `shouldSatisfy` isJust
      it "rejects negative minActionRepetitionDistance" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = Just (-1)
        } `shouldSatisfy` isJust
      it "accepts minActionRepetitionDistance >= 0 when cycles >= 1" $
        checkSelectASConfig defaultSelectASConfig {
          minActionRepetitionDistance = Just 1,
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
    context "when minActionRepetitionDistance = Nothing" $
      it "generates sequences without forcing repetition" $ do
        let solution = selectActionSequence Nothing 2 testDiagram
            actionSeq = correctSequence solution
        length actionSeq `shouldSatisfy` (>= 2)
    context "when minActionRepetitionDistance = Just 0" $
      it "generates sequences with immediate repetition when possible" $ do
        let solution = selectActionSequence (Just 0) 2 testDiagram
            actionSeq = correctSequence solution
        hasActionRepetitionWithMinDistance 0 actionSeq `shouldBe` True
    context "when minActionRepetitionDistance = Just 1" $
      it "generates sequences with at least 1 action between repetitions" $ do
        let solution = selectActionSequence (Just 1) 2 testDiagram
            actionSeq = correctSequence solution
        hasActionRepetitionWithMinDistance 1 actionSeq `shouldBe` True
