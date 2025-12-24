module Modelling.PetriNet.Reach.DeadlockSpec where

import Data.List.NonEmpty                 (toList)
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  defaultDeadlockInstance,
  generateDeadlock,
  checkDeadlockConfig,
  deadlockSyntax,
  )
import Modelling.PetriNet.Reach.Filter (
  areSolutionsTrivial,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type    (Net (transitions), Capacity(..), Place(..), Transition(..))

import Data.Maybe                       (isJust)
import qualified Data.Map                 as M
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )
import Control.OutputCapable.Blocks.Generic (runLangMReport)

import Settings (needsTuning)

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
              minTransitionLength = 6,
              filterConfig = noFiltering
              }
            minL = minTransitionLength config
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            ts = transitions net
        net `shouldSatisfy`
          hasMinTransitionLength (null . successors net) ts minL

    needsTuning $
      it "generates non-trivial solutions when filtering is enabled" $
        quickCheckWith stdArgs {maxSuccess = 15} $ property $ \seed -> do
          let config = defaultDeadlockConfig
          deadlockInstance <- generateDeadlock config seed
          let allSolutions = either undefined toList (shortestSolutions deadlockInstance)
              availableTransitions = transitions (petriNet deadlockInstance)
          allSolutions `shouldSatisfy` not . areSolutionsTrivial (filterConfig config) availableTransitions

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

  describe "deadlockSyntax" $ do
    it "rejects Spaceballs pattern when minSpaceballsLength is set" $ do
      let inst = defaultDeadlockInstance { minSpaceballsLength = Just 4 }
          spaceballsSequence = [Transition 1, Transition 2, Transition 3, Transition 4]
      result <- testDeadlockSyntax inst spaceballsSequence
      result `shouldSatisfy` not

    it "accepts non-Spaceballs pattern when minSpaceballsLength is set" $ do
      let inst = defaultDeadlockInstance { minSpaceballsLength = Just 4 }
          nonSpaceballsSequence = [Transition 1, Transition 3, Transition 2, Transition 4]
      result <- testDeadlockSyntax inst nonSpaceballsSequence
      result `shouldBe` True

    it "accepts Spaceballs pattern when minSpaceballsLength is Nothing" $ do
      let inst = defaultDeadlockInstance { minSpaceballsLength = Nothing }
          spaceballsSequence = [Transition 1, Transition 2, Transition 3, Transition 4]
      result <- testDeadlockSyntax inst spaceballsSequence
      result `shouldBe` True

    it "rejects Spaceballs prefix when sequence is longer" $ do
      let inst = defaultDeadlockInstance { minSpaceballsLength = Just 4 }
          sequenceWithSpaceballsPrefix = [Transition 1, Transition 2, Transition 3, Transition 4, Transition 1, Transition 3]
      result <- testDeadlockSyntax inst sequenceWithSpaceballsPrefix
      result `shouldSatisfy` not

testDeadlockSyntax :: DeadlockInstance Place Transition -> [Transition] -> IO Bool
testDeadlockSyntax inst transitionSequence = do
  (maybeResult, _output) <- runLangMReport (return () :: IO ()) (>>) (deadlockSyntax inst transitionSequence)
  return $ case maybeResult of
    Just () -> True
    Nothing -> False

