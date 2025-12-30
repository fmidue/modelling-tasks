module Modelling.PetriNet.Reach.ReachSpec where

import qualified Data.Set                         as S

import Data.List.NonEmpty                 (toList)
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.PetriNet.Reach.Reach (
  ReachConfig (..),
  NetGoalConfig (..),
  ReachInstance (..),
  NetGoal (..),
  defaultReachConfig,
  generateReach,
  checkReachConfig,
  )
import Modelling.PetriNet.Reach.Filter (
  shouldDiscardSolutions,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Property (
  satisfiesAtAnyState,
  )
import Modelling.PetriNet.Reach.Type (
  Net (transitions, start, connections),
  State,
  Transition (..),
  Capacity(..),
  Place(..),
  TransitionBehaviorConstraints(..),
  isTokenPreserving,
  isTokenIncreasing,
  isTokenDecreasing,
  mark,
  )

import Data.Maybe                        (isJust)
import qualified Data.Map                 as M
import Data.Set                         (Set)

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
  describe "generateReach" $ do
    it "abides minTransitionLength" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering
              }
            minL = minTransitionLength (netGoalConfig config)
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            s = goal (netGoal inst)
            ts = transitions net
        net `shouldSatisfy` hasMinTransitionLength (s ==) ts minL

    needsTuning $
     it "generates non-trivial solutions when filtering is enabled" $
      quickCheckWith stdArgs {maxSuccess = 15} $ property $ \seed -> do
        let config = defaultReachConfig
        inst <- generateReach config seed
        let allSolutions = either undefined toList (shortestSolutions inst)
        allSolutions `shouldSatisfy` not . shouldDiscardSolutions (filterConfig config) (numTransitions $ netGoalConfig config)

    it "adheres to maxPlacesChanged constraint with noFiltering" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxPlacesChanged = 2
                }
              }
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            startState = start net
            goalState = goal (netGoal inst)
            numberOfPlaces = numPlaces $ netGoalConfig config
            places = [Place 1 .. Place numberOfPlaces]
            numberOfDifferentPlaces = length $ filter (\p -> mark startState p /= mark goalState p) places
        numberOfDifferentPlaces `shouldSatisfy` (<= 2)

  describe "checkReachConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultReachConfig
      checkReachConfig config `shouldBe` Nothing

    it "rejects preconditionsRange where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              preconditionsRange = (5, Just 2)
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects postconditionsRange where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              postconditionsRange = (5, Just 2)
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects empty drawCommands" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              drawCommands = []
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts Unbounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = Unbounded
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects AllBounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = AllBounded 5
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects Bounded capacity" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              capacity = Bounded (M.fromList [(Place 1, 3), (Place 2, 5)])
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration with both showTargetNet = False and showPlaceNamesInNet = False" $ do
      let config = defaultReachConfig {
            showTargetNet = False,
            showPlaceNamesInNet = False
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts valid transitionBehaviorConstraints with exactlyNonPreserving" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Nothing,
                exactlyNonPreserving = Just 2
                }
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects negative exactlyNonPreserving" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Nothing,
                exactlyNonPreserving = Just (-1)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects exactlyNonPreserving greater than numTransitions" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Nothing,
                exactlyNonPreserving = Just 10
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChangeTypes = Just EQ (meaningless)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Just EQ,
                exactlyNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects meaningless combination: exactlyNonPreserving = 0 with allowedTokenChangeTypes" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChangeTypes = Just GT,
                exactlyNonPreserving = Just 0
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "respects allowedTokenChangeTypes = Just LT (only token-decreasing)" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChangeTypes = Just LT,
                  exactlyNonPreserving = Nothing
                  }
                }
              }
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            increasingCount = length $ filter isTokenIncreasing $ connections net
        increasingCount `shouldBe` 0

    it "respects allowedTokenChangeTypes = Just GT (only token-increasing)" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChangeTypes = Just GT,
                  exactlyNonPreserving = Nothing
                  }
                }
              }
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            decreasingCount = length $ filter isTokenDecreasing $ connections net
        decreasingCount `shouldBe` 0

    it "respects exactlyNonPreserving constraint" $
      quickCheckWith stdArgs {maxSuccess = 50} $ property $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChangeTypes = Nothing,
                  exactlyNonPreserving = Just 2
                  }
                }
              }
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            nonPreservingCount = length $ filter (not . isTokenPreserving) $ connections net
        nonPreservingCount `shouldBe` 2

hasMinTransitionLength
  :: (Ord s, Show s)
  => (State s -> Bool)
  -> Set Transition
  -> Int
  -> Net s Transition
  -> Bool
hasMinTransitionLength p ts minL n =
  not (any (satisfiesAtAnyState p n) variants)
  where
    variants = transitionVariants $ minL - 1
    transitionVariants x
      | x < 1     = [[]]
      | otherwise = [ a : as |
          a <- S.toList ts,
          as <- transitionVariants (x-1)
          ]
