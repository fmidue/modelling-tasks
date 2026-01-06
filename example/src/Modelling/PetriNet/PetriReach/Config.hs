-- |

module Modelling.PetriNet.PetriReach.Config where

import Modelling.PetriNet.Reach.Reach   (ReachConfig(..), NetGoalConfig(..))
import Modelling.PetriNet.Reach.Filter  (defaultFilterConfig, FilterConfig(absentTransitionsRequirement, forbiddenCycleLengths, requireCycleLengthsAny))
import Modelling.PetriNet.Reach.Type    (Capacity(..), noTransitionBehaviorConstraints)
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.2
-}
task2023_27 :: ReachConfig
task2023_27 = ReachConfig {
  netGoalConfig  = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    drawPreferenceOrder = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    incomingArrowsPerTransition = (2, Just 2),
    outgoingArrowsPerTransition = (2, Just 3),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (0, Nothing),
    totalArrowsFromTransitionsToPlaces = (0, Nothing),
    maxPlacesChanged = 4,
    transitionBehaviorConstraints = noTransitionBehaviorConstraints
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
    drawPreferenceOrder = [Circo],
    maxTransitionLength = 12,
    minTransitionLength = 12,
    incomingArrowsPerTransition = (2, Just 2),
    outgoingArrowsPerTransition = (2, Just 3),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (0, Nothing),
    totalArrowsFromTransitionsToPlaces = (0, Nothing),
    maxPlacesChanged = 6,
    transitionBehaviorConstraints = noTransitionBehaviorConstraints
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
    drawPreferenceOrder = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    incomingArrowsPerTransition = (2, Just 2),
    outgoingArrowsPerTransition = (2, Just 3),
    incomingArrowsPerPlace = (0, Nothing),
    outgoingArrowsPerPlace = (0, Nothing),
    totalArrowsFromPlacesToTransitions = (0, Nothing),
    totalArrowsFromTransitionsToPlaces = (0, Nothing),
    maxPlacesChanged = 4,
    transitionBehaviorConstraints = noTransitionBehaviorConstraints
    },
  maxPrintedSolutions = 10,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig { absentTransitionsRequirement = 0, forbiddenCycleLengths = [], requireCycleLengthsAny = [] }
  }
