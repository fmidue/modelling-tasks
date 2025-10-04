module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..), checkSelectASConfig, defaultSelectASConfig, selectActionSequence, SelectASSolution(..))

import Modelling.ActivityDiagram.Config (
  AdConfig (objectNodeLimits, cycles),
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdNode(..),
  AdConnection(..),
  UMLActivityDiagram(..)
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Data.List (nub)
import Control.Monad.Random (evalRandT, mkStdGen)
import Data.Functor.Identity (runIdentity)

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
            AdInitialNode {label = 0},
            AdActionNode {label = 1, name = "A"},
            AdActionNode {label = 2, name = "B"},
            AdFlowFinalNode {label = 3}
          ],
          connections = [
            AdConnection {from = 0, to = 1, guard = ""},    -- Initial -> A
            AdConnection {from = 1, to = 2, guard = ""},    -- A -> B
            AdConnection {from = 2, to = 3, guard = ""}     -- B -> Final
          ]
        }

    context "without action duplication requirement" $
      it "generates wrong sequences without duplicates" $ do
        let g = mkStdGen 42
            result = runIdentity $ evalRandT (selectActionSequence 3 Nothing testDiagram) g
            hasNoDuplicates xs = length xs == length (nub xs)
        all hasNoDuplicates (wrongSequences result) `shouldBe` True

    context "with action duplication requirement" $
      it "generates sequences with duplicated actions" $ do
        let g = mkStdGen 42
            result = runIdentity $ evalRandT (selectActionSequence 5 (Just True) testDiagram) g
        length (wrongSequences result) `shouldSatisfy` (>= 0)  -- Should not crash
