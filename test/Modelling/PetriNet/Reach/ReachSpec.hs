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
  ArrowDensityConstraints(..),
  connectionTokenBehavior,
  mark,
  noTransitionBehaviorConstraints,
  )

import Data.Maybe                        (isJust)
import qualified Data.Map                 as M
import Data.Set                         (Set)

import Settings (nightly)

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

spec :: Spec
spec = do
  describe "generateReach" $ do
    modifyMaxSuccess (const 15) $
      prop "abides minTransitionLength" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
                }
              }
            minL = minTransitionLength (netGoalConfig config)
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            s = goal (netGoal inst)
            ts = transitions net
        net `shouldSatisfy` hasMinTransitionLength (s ==) ts minL

    nightly $
     modifyMaxSuccess (const 3) $
      prop "generates non-trivial solutions when filtering is enabled (as in the default configuration)" $ \seed -> do
        let config = defaultReachConfig
        inst <- generateReach config seed
        let allSolutions = either undefined toList (shortestSolutions inst)
        allSolutions `shouldSatisfy` not . shouldDiscardSolutions (filterConfig config) (numTransitions $ netGoalConfig config)

    modifyMaxSuccess (const 15) $
      prop "adheres to maxPlacesChanged constraint with noFiltering" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                maxPlacesChanged = 2,
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
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

    modifyMaxSuccess (const 5) $
      prop "respects incomingArrowsPerPlace constraint" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                arrowDensityConstraints = ArrowDensityConstraints {
                  incomingArrowsPerTransition = (0, Just 3),
                  outgoingArrowsPerTransition = (0, Just 3),
                  incomingArrowsPerPlace = (1, Just 2),
                  outgoingArrowsPerPlace = (0, Nothing),
                  totalArrowsFromPlacesToTransitions = (0, Nothing),
                  totalArrowsFromTransitionsToPlaces = (6, Just 12)
                  },
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            places = [Place 1 .. Place (numPlaces $ netGoalConfig config)]
            incomingArrowsPerPlaceList = map (\p -> countIncomingToPlace p (connections net)) places
        all (\count -> count >= 1 && count <= 2) incomingArrowsPerPlaceList `shouldBe` True

    modifyMaxSuccess (const 5) $
      prop "respects outgoingArrowsPerPlace constraint" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                arrowDensityConstraints = ArrowDensityConstraints {
                  incomingArrowsPerTransition = (0, Just 3),
                  outgoingArrowsPerTransition = (0, Just 3),
                  incomingArrowsPerPlace = (0, Nothing),
                  outgoingArrowsPerPlace = (1, Just 2),
                  totalArrowsFromPlacesToTransitions = (6, Just 12),
                  totalArrowsFromTransitionsToPlaces = (0, Nothing)
                  },
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            places = [Place 1 .. Place (numPlaces $ netGoalConfig config)]
            outgoingArrowsPerPlaceList = map (\p -> countOutgoingFromPlace p (connections net)) places
        all (\count -> count >= 1 && count <= 2) outgoingArrowsPerPlaceList `shouldBe` True

    modifyMaxSuccess (const 5) $
      prop "respects totalArrowsFromPlacesToTransitions constraint" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                arrowDensityConstraints = ArrowDensityConstraints {
                  incomingArrowsPerTransition = (0, Just 3),
                  outgoingArrowsPerTransition = (0, Just 3),
                  incomingArrowsPerPlace = (0, Nothing),
                  outgoingArrowsPerPlace = (0, Nothing),
                  totalArrowsFromPlacesToTransitions = (10, Just 18),
                  totalArrowsFromTransitionsToPlaces = (0, Nothing)
                  },
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            totalArrows = sum [length pre | (pre, _, _) <- connections net]
        totalArrows `shouldSatisfy` (\x -> x >= 10 && x <= 18)

    modifyMaxSuccess (const 5) $
      prop "respects totalArrowsFromTransitionsToPlaces constraint" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                arrowDensityConstraints = ArrowDensityConstraints {
                  incomingArrowsPerTransition = (0, Just 3),
                  outgoingArrowsPerTransition = (0, Just 3),
                  incomingArrowsPerPlace = (0, Nothing),
                  outgoingArrowsPerPlace = (0, Nothing),
                  totalArrowsFromPlacesToTransitions = (0, Nothing),
                  totalArrowsFromTransitionsToPlaces = (10, Just 18)
                  },
                transitionBehaviorConstraints = noTransitionBehaviorConstraints
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            totalArrows = sum [length post | (_, _, post) <- connections net]
        totalArrows `shouldSatisfy` (\x -> x >= 10 && x <= 18)

  describe "checkReachConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultReachConfig
      checkReachConfig config `shouldBe` Nothing

    it "rejects incomingArrowsPerTransition where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (5, Just 2)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects outgoingArrowsPerTransition where upper < lower" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                outgoingArrowsPerTransition = (5, Just 2)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects empty drawPreferenceOrder" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              drawPreferenceOrder = []
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

    it "accepts valid transitionBehaviorConstraints with areNonPreserving set to 0" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Nothing,
                areNonPreserving = Just 0
                }
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects negative areNonPreserving" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Nothing,
                areNonPreserving = Just (-1)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects areNonPreserving greater than numTransitions" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Nothing,
                areNonPreserving = Just 10
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChanges = Just EQ (meaningless)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just EQ,
                areNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects meaningless combination: areNonPreserving = 0 with allowedTokenChanges" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just GT,
                areNonPreserving = Just 0
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    modifyMaxSuccess (const 3) $
      prop "respects allowedTokenChanges = Just LT (only token-decreasing)" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChanges = Just LT,
                  areNonPreserving = Nothing
                  }
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            increasingCount = length $ filter (uncurry (<) . connectionTokenBehavior) $ connections net
        increasingCount `shouldBe` 0

    modifyMaxSuccess (const 3) $
      prop "respects allowedTokenChanges = Just GT (only token-increasing)" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChanges = Just GT,
                  areNonPreserving = Nothing
                  }
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            decreasingCount = length $ filter (uncurry (>) . connectionTokenBehavior) $ connections net
        decreasingCount `shouldBe` 0

    modifyMaxSuccess (const 1) $
      prop "respects areNonPreserving constraint set to 0" $ \seed -> do
        let config = defaultReachConfig {
              filterConfig = noFiltering,
              netGoalConfig = (netGoalConfig defaultReachConfig) {
                transitionBehaviorConstraints = TransitionBehaviorConstraints {
                  allowedTokenChanges = Nothing,
                  areNonPreserving = Just 0
                  }
                }
              }
        checkReachConfig config `shouldBe` Nothing
        inst <- generateReach config seed
        let net = petriNet (netGoal inst)
            nonPreservingCount = length $ filter (uncurry (/=) . connectionTokenBehavior) $ connections net
        nonPreservingCount `shouldBe` 0

    it "rejects allowedTokenChanges = Just LT with impossible range (vHigh <= nLow)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (1, Just 2),
                outgoingArrowsPerTransition = (3, Just 5)
                },
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just LT,
                areNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChanges = Just GT with impossible range (nHigh <= vLow)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (3, Just 5),
                outgoingArrowsPerTransition = (1, Just 2)
                },
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just GT,
                areNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects areNonPreserving > 0 with fixed equal ranges" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (2, Just 2),
                outgoingArrowsPerTransition = (2, Just 2)
                },
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Nothing,
                areNonPreserving = Just 1
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts allowedTokenChanges = Just LT with valid range" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (2, Just 5),
                outgoingArrowsPerTransition = (0, Just 3),
                totalArrowsFromPlacesToTransitions = (12, Just 30),
                totalArrowsFromTransitionsToPlaces = (0, Just 18)
                },
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just LT,
                areNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "accepts allowedTokenChanges = Just GT with valid range" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (0, Just 3),
                outgoingArrowsPerTransition = (2, Just 5),
                totalArrowsFromPlacesToTransitions = (0, Just 18),
                totalArrowsFromTransitionsToPlaces = (12, Just 30)
                },
              transitionBehaviorConstraints = TransitionBehaviorConstraints {
                allowedTokenChanges = Just GT,
                areNonPreserving = Nothing
                }
              }
            }
      checkReachConfig config `shouldBe` Nothing

    it "rejects configuration when totalArrowsFromPlacesToTransitions lower bound exceeds maximum possible from incomingArrowsPerTransition" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (0, Just 2),
                totalArrowsFromPlacesToTransitions = (20, Nothing)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration when totalArrowsFromTransitionsToPlaces lower bound exceeds maximum possible from outgoingArrowsPerTransition" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                outgoingArrowsPerTransition = (0, Just 2),
                totalArrowsFromTransitionsToPlaces = (20, Nothing)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration when totalArrowsFromPlacesToTransitions upper bound is less than minimum from incomingArrowsPerTransition" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (2, Just 3),
                totalArrowsFromPlacesToTransitions = (0, Just 5)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration when totalArrowsFromTransitionsToPlaces upper bound is less than minimum from outgoingArrowsPerTransition" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                outgoingArrowsPerTransition = (2, Just 3),
                totalArrowsFromTransitionsToPlaces = (0, Just 5)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration when totalArrowsFromPlacesToTransitions lower bound is less than minimum from incomingArrowsPerTransition (aggressive narrowing)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (2, Just 3),
                totalArrowsFromPlacesToTransitions = (5, Just 20)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "rejects configuration when totalArrowsFromPlacesToTransitions upper bound is greater than maximum from incomingArrowsPerTransition (aggressive narrowing)" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (1, Just 2),
                totalArrowsFromPlacesToTransitions = (6, Just 20)
                }
              }
            }
      checkReachConfig config `shouldSatisfy` isJust

    it "accepts configuration with consistent arrow density parameters" $ do
      let config = defaultReachConfig {
            netGoalConfig = (netGoalConfig defaultReachConfig) {
              arrowDensityConstraints = (arrowDensityConstraints $ netGoalConfig defaultReachConfig) {
                incomingArrowsPerTransition = (1, Just 2),
                outgoingArrowsPerTransition = (1, Just 2),
                incomingArrowsPerPlace = (1, Just 3),
                outgoingArrowsPerPlace = (1, Just 3),
                totalArrowsFromPlacesToTransitions = (6, Just 12),
                totalArrowsFromTransitionsToPlaces = (6, Just 12)
                }
              }
            }
      checkReachConfig config `shouldBe` Nothing

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

countIncomingToPlace :: Place -> [([Place], t, [Place])] -> Int
countIncomingToPlace place conns =
  sum [length $ filter (== place) post | (_, _, post) <- conns]

countOutgoingFromPlace :: Place -> [([Place], t, [Place])] -> Int
countOutgoingFromPlace place conns =
  sum [length $ filter (== place) pre | (pre, _, _) <- conns]
