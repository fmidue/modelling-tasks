-- |

module Modelling.PetriNet.PetriDeadlock.Config where

import Modelling.PetriNet.Reach.Deadlock (DeadlockConfig(..))
import Modelling.PetriNet.Reach.Filter  (defaultFilterConfig, FilterConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..), TransitionBehaviorConstraints(..), ArrowDensityConstraints(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))
import Data.Ratio                       ((%))

{-|
points: 0.2
-}
task2023_29 :: DeadlockConfig
task2023_29 = DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  graphLayouts = [Circo],
  maxTransitionLength = 7,
  minTransitionLength = 7,
  transitionBehaviorConstraints = TransitionBehaviorConstraints { allowedTokenChanges = Nothing, areNonPreserving = Nothing },
  arrowDensityConstraints = ArrowDensityConstraints {
    incomingArrowsPerTransition = (1, Just 2),
    outgoingArrowsPerTransition = (1, Just 2),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (4, Just 8),
    totalArrowsFromTransitionsToPlaces = (4, Just 8)
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 7,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Nothing,
  requireFusableOutputNodes = Nothing,
  filterConfig = defaultFilterConfig { forbiddenCycleLengths = [], absentTransitionsRequirement = 0, requireCycleLengthsAny = [] }
  }

{-|
points: 0.25
-}
task2023_30 :: DeadlockConfig
task2023_30 = DeadlockConfig {
  numPlaces = 6,
  numTransitions = 8,
  capacity = Unbounded,
  graphLayouts = [Circo],
  maxTransitionLength = 14,
  minTransitionLength = 14,
  transitionBehaviorConstraints = TransitionBehaviorConstraints { allowedTokenChanges = Nothing, areNonPreserving = Nothing },
  arrowDensityConstraints = ArrowDensityConstraints {
    incomingArrowsPerTransition = (1, Just 2),
    outgoingArrowsPerTransition = (1, Just 2),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (8, Just 16),
    totalArrowsFromTransitionsToPlaces = (8, Just 16)
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 14,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Nothing,
  requireFusableOutputNodes = Nothing,
  filterConfig = defaultFilterConfig { forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }

{-|
points: 0.2
-}
task2024_27 :: DeadlockConfig
task2024_27 = task2023_29

{-|
points: 0.25
average generation time per instance: 1:49min
CPU usage: 99%
-}
task2024_28 :: DeadlockConfig
task2024_28 = task2023_30

{-|
points: 0.08
-}
task2024_61 :: DeadlockConfig
task2024_61 = DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  graphLayouts = [Circo],
  maxTransitionLength = 8,
  minTransitionLength = 8,
  transitionBehaviorConstraints = TransitionBehaviorConstraints { allowedTokenChanges = Nothing, areNonPreserving = Nothing },
  arrowDensityConstraints = ArrowDensityConstraints {
    incomingArrowsPerTransition = (1, Just 2),
    outgoingArrowsPerTransition = (1, Just 2),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (4, Just 8),
    totalArrowsFromTransitionsToPlaces = (4, Just 8)
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Nothing,
  requireFusableOutputNodes = Nothing,
  filterConfig = defaultFilterConfig { absentTransitionsRequirement = 0, forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }

task2025_29 :: DeadlockConfig
task2025_29 = DeadlockConfig {
  numPlaces = 4,
  numTransitions = 4,
  capacity = Unbounded,
  graphLayouts = [Circo],
  maxTransitionLength = 7,
  minTransitionLength = 7,
  transitionBehaviorConstraints = TransitionBehaviorConstraints {
    allowedTokenChanges = Nothing,
    areNonPreserving = Nothing
    },
  arrowDensityConstraints = ArrowDensityConstraints {
    incomingArrowsPerTransition = (1, Just 2),
    outgoingArrowsPerTransition = (1, Just 2),
    incomingArrowsPerPlace = (1, Nothing),
    outgoingArrowsPerPlace = (1, Nothing),
    totalArrowsFromPlacesToTransitions = (5, Just 5),
    totalArrowsFromTransitionsToPlaces = (4, Just 6)
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 7,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Just 1,
  requireFusableOutputNodes = Nothing,
  filterConfig = FilterConfig {
    rejectGroupedRepeats = True,
    repetitiveSubsequenceThreshold = Just 3,
    spaceballsPrefixThreshold = Just 4,
    forbiddenCycleLengths = [],
    requireCycleLengthsAny = [],
    solutionSetLimit = Just 15,
    requireSolutionsArePermutations = False,
    absentTransitionsRequirement = 0,
    transitionCoverageRequirement = 3 % 4
    }
  }

task2025_30 :: DeadlockConfig
task2025_30 = DeadlockConfig {
  numPlaces = 6,
  numTransitions = 8,
  capacity = Unbounded,
  graphLayouts = [Circo],
  maxTransitionLength = 14,
  minTransitionLength = 14,
  transitionBehaviorConstraints = TransitionBehaviorConstraints {
    allowedTokenChanges = Nothing,
    areNonPreserving = Nothing
    },
  arrowDensityConstraints = ArrowDensityConstraints {
    incomingArrowsPerTransition = (1, Just 2),
    outgoingArrowsPerTransition = (1, Just 2),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (8, Just 16),
    totalArrowsFromTransitionsToPlaces = (8, Just 16)
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 14,
  showLengthHint = False,
  showMinLengthHint = True,
  showPlaceNamesInNet = False,
  requireFusableInputNodes = Nothing,
  requireFusableOutputNodes = Nothing,
  filterConfig = FilterConfig {
    rejectGroupedRepeats = True,
    repetitiveSubsequenceThreshold = Just 3,
    spaceballsPrefixThreshold = Just 4,
    forbiddenCycleLengths = [],
    requireCycleLengthsAny = [],
    solutionSetLimit = Just 15,
    requireSolutionsArePermutations = True,
    absentTransitionsRequirement = 0,
    transitionCoverageRequirement = 4 % 5
    }
  }
