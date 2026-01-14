-- |

module Modelling.PetriNet.PetriReach.Config where

import Modelling.PetriNet.Reach.Reach   (ReachConfig(..), NetGoalConfig(..))
import Modelling.PetriNet.Reach.Filter  (defaultFilterConfig, FilterConfig(..))
import Modelling.PetriNet.Reach.Type    (Capacity(..), TransitionBehaviorConstraints(..), ArrowDensityConstraints(..))
import Data.GraphViz.Commands           (GraphvizCommand(..))
import Data.Ratio                       ((%))

{-|
points: 0.2
-}
task2023_27 :: ReachConfig
task2023_27 = ReachConfig {
  netGoalConfig  = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    graphLayouts = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    arrowDensityConstraints = ArrowDensityConstraints {
      incomingArrowsPerTransition = (2, Just 2),
      outgoingArrowsPerTransition = (2, Just 3),
      incomingArrowsPerPlace = (0, Nothing),
      outgoingArrowsPerPlace = (0, Nothing),
      totalArrowsFromPlacesToTransitions = (8, Just 8),
      totalArrowsFromTransitionsToPlaces = (8, Just 12)
      },
    maxPlacesChanged = 4,
    transitionBehaviorConstraints = TransitionBehaviorConstraints {
      allowedTokenChanges = Nothing,
      areNonPreserving = Nothing
      }
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig { absentTransitionsRequirement = 0, forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }

{-|
points: 0.25
-}
task2023_28 :: ReachConfig
task2023_28 = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces = 6,
    numTransitions = 6,
    capacity = Unbounded,
    graphLayouts = [Circo],
    maxTransitionLength = 12,
    minTransitionLength = 12,
    arrowDensityConstraints = ArrowDensityConstraints {
      incomingArrowsPerTransition = (2, Just 2),
      outgoingArrowsPerTransition = (2, Just 3),
      incomingArrowsPerPlace = (0, Nothing),
      outgoingArrowsPerPlace = (0, Nothing),
      totalArrowsFromPlacesToTransitions = (12, Just 12),
      totalArrowsFromTransitionsToPlaces = (12, Just 18)
      },
    maxPlacesChanged = 6,
    transitionBehaviorConstraints = TransitionBehaviorConstraints {
      allowedTokenChanges = Nothing,
      areNonPreserving = Nothing
      }
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 12,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig { forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }

{-|
points: 0.2
-}
task2024_25 :: ReachConfig
task2024_25 = task2023_27

{-|
points: 0.25
-}
task2024_26 :: ReachConfig
task2024_26 = task2023_28

{-|
points: 0.08
-}
task2024_60 :: ReachConfig
task2024_60 = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    graphLayouts = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    arrowDensityConstraints = ArrowDensityConstraints {
      incomingArrowsPerTransition = (2, Just 2),
      outgoingArrowsPerTransition = (2, Just 3),
      incomingArrowsPerPlace = (0, Nothing),
      outgoingArrowsPerPlace = (0, Nothing),
      totalArrowsFromPlacesToTransitions = (8, Just 8),
      totalArrowsFromTransitionsToPlaces = (8, Just 12)
      },
    maxPlacesChanged = 4,
    transitionBehaviorConstraints = TransitionBehaviorConstraints {
      allowedTokenChanges = Nothing,
      areNonPreserving = Nothing
      }
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig { absentTransitionsRequirement = 0, forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }

task2025_27 :: ReachConfig
task2025_27 = ReachConfig {
  netGoalConfig  = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    graphLayouts = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    arrowDensityConstraints = ArrowDensityConstraints {
      incomingArrowsPerTransition = (2, Just 3),
      outgoingArrowsPerTransition = (2, Just 3),
      incomingArrowsPerPlace = (1, Nothing),
      outgoingArrowsPerPlace = (1, Nothing),
      totalArrowsFromPlacesToTransitions = (8, Just 8),
      totalArrowsFromTransitionsToPlaces = (8, Just 10)
      },
    maxPlacesChanged = 1,
    transitionBehaviorConstraints = TransitionBehaviorConstraints {
      allowedTokenChanges = Just GT,
      areNonPreserving = Just 1
      }
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = FilterConfig {
    rejectGroupedRepeats = True,
    repetitiveSubsequenceThreshold = Just 4,
    spaceballsPrefixThreshold = Just 4,
    forbiddenCycleLengths = [4],
    requireCycleLengthsAny = [],
    solutionSetLimit = Just 15,
    requireSolutionsArePermutations = True,
    absentTransitionsRequirement = 0,
    transitionCoverageRequirement = 3 % 4
    }
  }

task2025_28 :: ReachConfig
task2025_28 = ReachConfig {
  netGoalConfig  = NetGoalConfig {
    numPlaces = 6,
    numTransitions = 6,
    capacity = Unbounded,
    graphLayouts = [Circo],
    maxTransitionLength = 12,
    minTransitionLength = 12,
    arrowDensityConstraints = ArrowDensityConstraints {
      incomingArrowsPerTransition = (2, Just 2),
      outgoingArrowsPerTransition = (2, Just 3),
      incomingArrowsPerPlace = (0, Nothing),
      outgoingArrowsPerPlace = (0, Nothing),
      totalArrowsFromPlacesToTransitions = (12, Just 12),
      totalArrowsFromTransitionsToPlaces = (12, Just 18)
      },
    maxPlacesChanged = 1,
    transitionBehaviorConstraints = TransitionBehaviorConstraints {
      allowedTokenChanges = Just GT,
      areNonPreserving = Just 1
      }
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 12,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = FilterConfig {
    rejectGroupedRepeats = True,
    repetitiveSubsequenceThreshold = Just 3,
    spaceballsPrefixThreshold = Just 4,
    forbiddenCycleLengths = [],
    requireCycleLengthsAny = [6],
    solutionSetLimit = Just 15,
    requireSolutionsArePermutations = True,
    absentTransitionsRequirement = 1,
    transitionCoverageRequirement = 4 % 5
    }
  }
