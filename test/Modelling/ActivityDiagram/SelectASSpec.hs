module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..), checkSelectASConfig, defaultSelectASConfig, selectActionSequence, SelectASSolution(..))

import Modelling.ActivityDiagram.Config (
  AdConfig (objectNodeLimits, cycles),
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdNode(..), 
  UMLActivityDiagram(..)
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Data.List (nub)

spec :: Spec
spec = do
  describe "checkSelectASConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkSelectASConfig defaultSelectASConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkSelectASConfig defaultSelectASConfig {
          adConfig = defaultAdConfig {objectNodeLimits = (0, 1)},
          objectNodeOnEveryPath = Just True
        } `shouldSatisfy` isJust
    context "when requireActionDuplication is enabled" $ do
      it "requires cycles to be present" $
        checkSelectASConfig defaultSelectASConfig {
          adConfig = defaultAdConfig {cycles = 0},
          requireActionDuplication = Just True
        } `shouldSatisfy` isJust
      it "allows requireActionDuplication with cycles" $
        checkSelectASConfig defaultSelectASConfig {
          adConfig = defaultAdConfig {cycles = 1},
          requireActionDuplication = Just True
        } `shouldBe` Nothing

  describe "selectActionSequence" $ do
    let testDiagram = UMLActivityDiagram {
          nodes = [
            AdActionNode {label = 1, name = "A"},
            AdActionNode {label = 2, name = "B"},
            AdActionNode {label = 3, name = "C"}
          ],
          connections = []
        }
    
    context "without action duplication requirement" $
      it "generates wrong sequences without duplicates" $ do
        let result = selectActionSequence 3 Nothing testDiagram
            hasNoDuplicates xs = length xs == length (nub xs)
        all hasNoDuplicates (wrongSequences result) `shouldBe` True
    
    context "with action duplication requirement" $
      it "can generate wrong sequences with duplicates" $ do
        let result = selectActionSequence 10 (Just True) testDiagram
            hasDuplicates xs = length xs /= length (nub xs)
        -- Should have at least one sequence with duplicates
        length (wrongSequences result) `shouldSatisfy` (> 0)
        -- Check that the generation includes duplicated sequences
        any hasDuplicates (wrongSequences result) `shouldBe` True
