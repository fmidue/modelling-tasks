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
  connectionTokenBehavior,
  )

import Data.Maybe                       (isJust)
import qualified Data.Map                 as M
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

-- import Settings (needsTuning)

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

    -- needsTuning $
    it "generates non-trivial solutions when filtering is enabled" $
        quickCheckWith stdArgs {maxSuccess = 1} $ property $ \seed -> do
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

    it "accepts valid transitionBehaviorConstraints with areNonPreserving" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Nothing,
              areNonPreserving = Just 2
              }
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects negative areNonPreserving" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Nothing,
              areNonPreserving = Just (-1)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects areNonPreserving greater than numTransitions" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Nothing,
              areNonPreserving = Just 10
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChanges = Just EQ (meaningless)" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just EQ,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects meaningless combination: areNonPreserving = 0 with allowedTokenChanges" $ do
      let config = defaultDeadlockConfig {
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just GT,
              areNonPreserving = Just 0
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    -- needsTuning $
    it "respects allowedTokenChanges = Just LT (only token-decreasing)" $
      quickCheckWith stdArgs {maxSuccess = 1} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just LT,
                areNonPreserving = Nothing
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            increasingCount = length $ filter (uncurry (<) . connectionTokenBehavior) $ connections net
        increasingCount `shouldBe` 0

    -- needsTuning $
    it "respects allowedTokenChanges = Just GT (only token-increasing)" $
      quickCheckWith stdArgs {maxSuccess = 1} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just GT,
                areNonPreserving = Nothing
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            decreasingCount = length $ filter (uncurry (>) . connectionTokenBehavior) $ connections net
        decreasingCount `shouldBe` 0

    -- needsTuning $
    it "respects areNonPreserving constraint" $
      quickCheckWith stdArgs {maxSuccess = 1} $ property $ \seed -> do
        let config = defaultDeadlockConfig {
              filterConfig = noFiltering,
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Nothing,
                areNonPreserving = Just 0
                }
              }
        inst <- generateDeadlock config seed
        let net = petriNet inst
            nonPreservingCount = length $ filter (uncurry (/=) . connectionTokenBehavior) $ connections net
        nonPreservingCount `shouldBe` 0

    it "rejects allowedTokenChanges = Just LT with impossible range (vHigh <= nLow)" $ do
      let config = defaultDeadlockConfig {
            preconditionsRange = (1, Just 2),
            postconditionsRange = (3, Just 5),
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just LT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChanges = Just GT with impossible range (nHigh <= vLow)" $ do
      let config = defaultDeadlockConfig {
            preconditionsRange = (3, Just 5),
            postconditionsRange = (1, Just 2),
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just GT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects areNonPreserving > 0 with fixed equal ranges" $ do
      let config = defaultDeadlockConfig {
            preconditionsRange = (2, Just 2),
            postconditionsRange = (2, Just 2),
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Nothing,
              areNonPreserving = Just 1
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts allowedTokenChanges = Just LT with valid range" $ do
      let config = defaultDeadlockConfig {
            preconditionsRange = (2, Just 5),
            postconditionsRange = (0, Just 3),
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just LT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts allowedTokenChanges = Just GT with valid range" $ do
      let config = defaultDeadlockConfig {
            preconditionsRange = (0, Just 3),
            postconditionsRange = (2, Just 5),
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just GT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldBe` Nothing
