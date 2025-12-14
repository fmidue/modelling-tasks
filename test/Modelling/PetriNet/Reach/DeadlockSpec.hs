module Modelling.PetriNet.Reach.DeadlockSpec where

import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  generateDeadlock,
  checkDeadlockConfig,
  deadlockAllSolutions,
  deadlockSolution,
  )
import Modelling.PetriNet.Reach.Filter (
  defaultFilterConfig,
  noFiltering,
  isTrivialSequence,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type    (Net (transitions), Capacity(..), Place(..))

import Data.Maybe                       (isJust)
import qualified Data.Map                 as M
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

import Test.Hspec
import Test.QuickCheck (
  Testable (property),
  maxSuccess,
  quickCheckWith,
  stdArgs,
  )

spec :: Spec
spec = do
  describe "generateDeadlock" $ do
    it "abides minTransitionLength" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6
              }
            minL = minTransitionLength config
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            ts = transitions net
        net `shouldSatisfy`
          hasMinTransitionLength (null . successors net) ts minL

    it "generates non-trivial solutions when filtering is enabled" $
      quickCheckWith stdArgs {maxSuccess = 15} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 8,
              minTransitionLength = 8,
              filterConfig = defaultFilterConfig
              }
        deadlockInstance <- generateDeadlock config seed
        let solutions = deadlockAllSolutions (petriNet deadlockInstance)
        solutions `shouldSatisfy` (not . any (isTrivialSequence $ filterConfig config))

    it "can generate solutions when filtering is disabled" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 8,
              minTransitionLength = 8,
              filterConfig = noFiltering
              }
        deadlockInstance <- generateDeadlock config seed
        let solution = deadlockSolution deadlockInstance
        length solution `shouldBe` 8

  describe "checkDeadlockConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultDeadlockConfig
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects preconditionsRange where upper < lower" $ do
      let config = defaultDeadlockConfig { preconditionsRange = (5, Just 2) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects postconditionsRange where upper < lower" $ do
      let config = defaultDeadlockConfig { postconditionsRange = (5, Just 2) }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects empty drawCommands" $ do
      let config = defaultDeadlockConfig { drawCommands = [] }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts Unbounded capacity" $ do
      let config = defaultDeadlockConfig {
            capacity = Unbounded
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects AllBounded capacity" $ do
      let config = defaultDeadlockConfig {
            capacity = AllBounded 5
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects Bounded capacity" $ do
      let config = defaultDeadlockConfig {
            capacity = Bounded (M.fromList [(Place 1, 3), (Place 2, 5)])
            }
      checkDeadlockConfig config `shouldSatisfy` isJust
