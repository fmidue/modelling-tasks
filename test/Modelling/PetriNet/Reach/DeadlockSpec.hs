module Modelling.PetriNet.Reach.DeadlockSpec where

import Data.List.NonEmpty                 (toList)
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Deadlock (
  DeadlockConfig (..),
  DeadlockInstance (..),
  defaultDeadlockConfig,
  generateDeadlock,
  checkDeadlockConfig,
  )
import Modelling.PetriNet.Reach.Filter (
  shouldDiscardSolutions,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Step    (successors)
import Modelling.PetriNet.Reach.Type (
  Net (transitions, connections),
  Capacity(..),
  Place(..),
  TransitionBehaviorConstraints(..),
  isTokenPreserving,
  isTokenIncreasing,
  isTokenDecreasing,
  )

import Data.Maybe                       (isJust)
import qualified Data.Map                 as M
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

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
          allSolutions `shouldSatisfy` not . shouldDiscardSolutions (filterConfig config) (numTransitions config)

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

    it "rejects empty drawPreferenceOrder" $ do
      let config = defaultDeadlockConfig { drawPreferenceOrder = [] }
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

    it "accepts valid transitionBehaviorConstraints with exactlyNonPreserving" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChangeTypes = Nothing,
              exactlyNonPreserving = Just 2
              }
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects negative exactlyNonPreserving" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChangeTypes = Nothing,
              exactlyNonPreserving = Just (-1)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects exactlyNonPreserving greater than numTransitions" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChangeTypes = Nothing,
              exactlyNonPreserving = Just 10
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChangeTypes = Just EQ (meaningless)" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChangeTypes = Just EQ,
              exactlyNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects meaningless combination: exactlyNonPreserving = 0 with allowedTokenChangeTypes" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChangeTypes = Just GT,
              exactlyNonPreserving = Just 0
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    needsTuning $
     it "respects allowedTokenChangeTypes = Just LT (only token-decreasing)" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Just LT,
                exactlyNonPreserving = Nothing
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            increasingCount = length $ filter isTokenIncreasing $ connections net
        increasingCount `shouldBe` 0

    needsTuning $
     it "respects allowedTokenChangeTypes = Just GT (only token-increasing)" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Just LT,
                exactlyNonPreserving = Nothing
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            decreasingCount = length $ filter isTokenDecreasing $ connections net
        decreasingCount `shouldBe` 0

    needsTuning $
     it "respects exactlyNonPreserving constraint" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Nothing,
                exactlyNonPreserving = Just 2
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            nonPreservingCount = length $ filter (not . isTokenPreserving) $ connections net
        nonPreservingCount `shouldBe` 2
