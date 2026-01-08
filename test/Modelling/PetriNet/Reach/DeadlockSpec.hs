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
  ArrowDensityConstraints(..),
  connectionTokenBehavior,
  countFusableInputNodes,
  countFusableOutputNodes,
  noArrowDensityConstraints,
  )

import Data.Maybe                       (isJust)
import qualified Data.Map                 as M
import Modelling.PetriNet.Reach.ReachSpec (
  hasMinTransitionLength,
  )

import Settings (nightly)

import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

spec :: Spec
spec = do
  describe "generateDeadlock" $ do
    modifyMaxSuccess (const 15) $
      prop "abides minTransitionLength" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              filterConfig = noFiltering
              }
            minL = minTransitionLength config
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            ts = transitions net
        net `shouldSatisfy`
          hasMinTransitionLength (null . successors net) ts minL

    nightly $
     modifyMaxSuccess (const 1) $
      prop "generates non-trivial solutions when filtering is enabled (as in the default configuration)" $ \seed -> do
          let config = defaultDeadlockConfig
          deadlockInstance <- generateDeadlock config seed
          let allSolutions = either undefined toList (shortestSolutions deadlockInstance)
          allSolutions `shouldSatisfy` not . shouldDiscardSolutions (filterConfig config) (numTransitions config)

    modifyMaxSuccess (const 3) $
      prop "respects incomingArrowsPerPlace constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              arrowDensityConstraints = noArrowDensityConstraints {
                incomingArrowsPerPlace = (1, Just 2),
                totalArrowsFromTransitionsToPlaces = (6, Just 12)
                },
              filterConfig = noFiltering
              }
            countIncomingToPlace :: Place -> [([Place], t, [Place])] -> Int
            countIncomingToPlace place conns =
              sum [length $ filter (== place) post | (_, _, post) <- conns]
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            places = [Place 1 .. Place (numPlaces config)]
            incomingArrowsPerPlaceList = map (\p -> countIncomingToPlace p (connections net)) places
        all (\count -> count >= 1 && count <= 2) incomingArrowsPerPlaceList `shouldBe` True

    modifyMaxSuccess (const 3) $
      prop "respects outgoingArrowsPerPlace constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              arrowDensityConstraints = noArrowDensityConstraints {
                outgoingArrowsPerPlace = (1, Just 2),
                totalArrowsFromPlacesToTransitions = (6, Just 12)
                },
              filterConfig = noFiltering
              }
            countOutgoingFromPlace :: Place -> [([Place], t, [Place])] -> Int
            countOutgoingFromPlace place conns =
              sum [length $ filter (== place) pre | (pre, _, _) <- conns]
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            places = [Place 1 .. Place (numPlaces config)]
            outgoingArrowsPerPlaceList = map (\p -> countOutgoingFromPlace p (connections net)) places
        all (\count -> count >= 1 && count <= 2) outgoingArrowsPerPlaceList `shouldBe` True

    modifyMaxSuccess (const 3) $
      prop "respects totalArrowsFromPlacesToTransitions constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              arrowDensityConstraints = noArrowDensityConstraints {
                totalArrowsFromPlacesToTransitions = (8, Just 15)
                },
              filterConfig = noFiltering
              }
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            totalArrows = sum [length pre | (pre, _, _) <- connections net]
        totalArrows `shouldSatisfy` (\x -> x >= 8 && x <= 15)

    modifyMaxSuccess (const 3) $
      prop "respects totalArrowsFromTransitionsToPlaces constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              arrowDensityConstraints = noArrowDensityConstraints {
                totalArrowsFromTransitionsToPlaces = (8, Just 15)
                },
              filterConfig = noFiltering
              }
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            totalArrows = sum [length post | (_, _, post) <- connections net]
        totalArrows `shouldSatisfy` (\x -> x >= 8 && x <= 15)

    let configLT = defaultDeadlockConfig {
          filterConfig = noFiltering,
          transitionBehaviorConstraints = TransitionBehaviorConstraints {
            allowedTokenChanges = Just LT,
            areNonPreserving = Nothing
            }
          }
    it "has valid config for nightly test (allowedTokenChanges LT)" $
      checkDeadlockConfig configLT `shouldBe` Nothing
    nightly $
     modifyMaxSuccess (const 1) $
      prop "respects allowedTokenChanges = Just LT (only token-decreasing)" $ \seed -> do
        inst <- generateDeadlock configLT seed
        let net = petriNet inst
            increasingCount = length $ filter (uncurry (<) . connectionTokenBehavior) $ connections net
        increasingCount `shouldBe` 0

    let configGT = defaultDeadlockConfig {
          filterConfig = noFiltering,
          transitionBehaviorConstraints = TransitionBehaviorConstraints {
            allowedTokenChanges = Just GT,
            areNonPreserving = Nothing
            }
          }
    it "has valid config for nightly test (allowedTokenChanges GT)" $
      checkDeadlockConfig configGT `shouldBe` Nothing
    nightly $
     modifyMaxSuccess (const 1) $
      prop "respects allowedTokenChanges = Just GT (only token-increasing)" $ \seed -> do
        inst <- generateDeadlock configGT seed
        let net = petriNet inst
            decreasingCount = length $ filter (uncurry (>) . connectionTokenBehavior) $ connections net
        decreasingCount `shouldBe` 0

    let configNP = defaultDeadlockConfig {
          filterConfig = noFiltering,
          transitionBehaviorConstraints = TransitionBehaviorConstraints {
            allowedTokenChanges = Nothing,
            areNonPreserving = Just 1
            }
          }
    it "has valid config for nightly test (areNonPreserving)" $
      checkDeadlockConfig configNP `shouldBe` Nothing
    nightly $
     modifyMaxSuccess (const 1) $
      prop "respects areNonPreserving constraint" $ \seed -> do
        inst <- generateDeadlock configNP seed
        let net = petriNet inst
            nonPreservingCount = length $ filter (uncurry (/=) . connectionTokenBehavior) $ connections net
        nonPreservingCount `shouldBe` 1

    modifyMaxSuccess (const 3) $
      prop "respects requireFusableInputNodes constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              requireFusableInputNodes = Just 2,
              filterConfig = noFiltering
              }
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            actualFusableInputCount = countFusableInputNodes net
        actualFusableInputCount `shouldBe` 2

    modifyMaxSuccess (const 3) $
      prop "respects requireFusableOutputNodes constraint" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              requireFusableOutputNodes = Just 2,
              filterConfig = noFiltering
              }
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            actualFusableOutputCount = countFusableOutputNodes net
        actualFusableOutputCount `shouldBe` 2

    modifyMaxSuccess (const 3) $
      prop "respects both fusable node constraints simultaneously" $ \seed -> do
        let config = defaultDeadlockConfig {
              maxTransitionLength = 6,
              minTransitionLength = 6,
              requireFusableInputNodes = Just 1,
              requireFusableOutputNodes = Just 1,
              filterConfig = noFiltering
              }
        checkDeadlockConfig config `shouldBe` Nothing
        deadlockInstance <- generateDeadlock config seed
        let net = petriNet deadlockInstance
            actualFusableInputCount = countFusableInputNodes net
            actualFusableOutputCount = countFusableOutputNodes net
        actualFusableInputCount `shouldBe` 1
        actualFusableOutputCount `shouldBe` 1

  describe "checkDeadlockConfig" $ do
    it "accepts valid configuration" $ do
      let config = defaultDeadlockConfig
      checkDeadlockConfig config `shouldBe` Nothing

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
              areNonPreserving = Just 1
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

    it "rejects allowedTokenChanges = Just LT with impossible range (vHigh <= nLow)" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = noArrowDensityConstraints {
              incomingArrowsPerTransition = (1, Just 2),
              outgoingArrowsPerTransition = (3, Just 5),
              totalArrowsFromPlacesToTransitions = (6, Just 12),
              totalArrowsFromTransitionsToPlaces = (18, Just 30)
              },
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just LT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects allowedTokenChanges = Just GT with impossible range (nHigh <= vLow)" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = noArrowDensityConstraints {
              incomingArrowsPerTransition = (3, Just 5),
              outgoingArrowsPerTransition = (1, Just 2),
              totalArrowsFromPlacesToTransitions = (18, Just 30),
              totalArrowsFromTransitionsToPlaces = (6, Just 12)
              },
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Just GT,
              areNonPreserving = Nothing
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects areNonPreserving > 0 with fixed equal ranges" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = noArrowDensityConstraints {
              incomingArrowsPerTransition = (2, Just 2),
              outgoingArrowsPerTransition = (2, Just 2),
              totalArrowsFromPlacesToTransitions = (12, Just 12),
              totalArrowsFromTransitionsToPlaces = (12, Just 12)
              },
            transitionBehaviorConstraints = TransitionBehaviorConstraints {
              allowedTokenChanges = Nothing,
              areNonPreserving = Just 1
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts allowedTokenChanges = Just LT with valid range" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = noArrowDensityConstraints {
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
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts allowedTokenChanges = Just GT with valid range" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = noArrowDensityConstraints {
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
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts configuration with consistent arrow density parameters" $ do
      let config = defaultDeadlockConfig {
            arrowDensityConstraints = ArrowDensityConstraints {
              incomingArrowsPerTransition = (1, Just 2),
              outgoingArrowsPerTransition = (1, Just 2),
              incomingArrowsPerPlace = (1, Just 3),
              outgoingArrowsPerPlace = (1, Just 3),
              totalArrowsFromPlacesToTransitions = (6, Just 12),
              totalArrowsFromTransitionsToPlaces = (6, Just 12)
              }
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts Nothing for requireFusableInputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Nothing
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts valid requireFusableInputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just 2
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects negative requireFusableInputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just (-1)
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects requireFusableInputNodes exceeding numTransitions" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just 10
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts Nothing for requireFusableOutputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Nothing
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "accepts valid requireFusableOutputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Just 2
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects negative requireFusableOutputNodes" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Just (-1)
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects requireFusableOutputNodes exceeding numTransitions" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Just 10
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "accepts both fusable node parameters set" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just 1,
            requireFusableOutputNodes = Just 1
            }
      checkDeadlockConfig config `shouldBe` Nothing

    it "rejects requireFusableInputNodes > 0 with incomingArrowsPerTransition minimum > 1" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just 1,
            arrowDensityConstraints = noArrowDensityConstraints {
              incomingArrowsPerTransition = (2, Just 3)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects requireFusableOutputNodes > 0 with outgoingArrowsPerTransition minimum > 1" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Just 1,
            arrowDensityConstraints = noArrowDensityConstraints {
              outgoingArrowsPerTransition = (2, Just 3)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects requireFusableInputNodes > 0 with outgoingArrowsPerPlace minimum > 1" $ do
      let config = defaultDeadlockConfig {
            requireFusableInputNodes = Just 1,
            arrowDensityConstraints = noArrowDensityConstraints {
              outgoingArrowsPerPlace = (2, Just 3)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects requireFusableOutputNodes > 0 with incomingArrowsPerPlace minimum > 1" $ do
      let config = defaultDeadlockConfig {
            requireFusableOutputNodes = Just 1,
            arrowDensityConstraints = noArrowDensityConstraints {
              incomingArrowsPerPlace = (2, Just 3)
              }
            }
      checkDeadlockConfig config `shouldSatisfy` isJust

    it "rejects when requireFusableInputNodes + requireFusableOutputNodes exceeds numTransitions" $ do
      let config = defaultDeadlockConfig {
            numTransitions = 5,
            requireFusableInputNodes = Just 3,
            requireFusableOutputNodes = Just 3
            }
      checkDeadlockConfig config `shouldSatisfy` isJust
