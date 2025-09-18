-- |

module Modelling.PetriNet.PetriReach.Config where

import Modelling.PetriNet.Reach.Reach   (ReachConfig(..), NetGoalConfig(..))
import Modelling.PetriNet.Reach.Filter  (FilterConfig(..), defaultFilterConfig)
import Modelling.PetriNet.Reach.Type    (Capacity(..))
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
    drawCommands = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    postconditionsRange = (2, Just 3),
    preconditionsRange = (2, Just 2)
    },
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig
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
    drawCommands = [Circo],
    maxTransitionLength = 12,
    minTransitionLength = 12,
    postconditionsRange = (2, Just 3),
    preconditionsRange = (2, Just 2)
    },
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig
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
    drawCommands = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    postconditionsRange = (2, Just 3),
    preconditionsRange = (2, Just 2)
    },
  printSolution = True,
  rejectLongerThan = Just 8,
  showLengthHint = False,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = False,
  filterConfig = defaultFilterConfig
  }

{-|
Example configuration with custom filter settings to exclude trivial sequences.
This configuration is more strict about filtering patterns to prevent students
from accidentally guessing correct answers.
-}
taskWithStrictFiltering :: ReachConfig
taskWithStrictFiltering = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    drawCommands = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    postconditionsRange = (2, Just 3),
    preconditionsRange = (2, Just 2)
    },
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = True,
  filterConfig = FilterConfig {
    filterCyclicPatterns = True,         -- Reject [t1,t2,t3,t4,t1,t2,t3,t4]
    filterRepetitiveSubsequences = True, -- Reject [t4,t4,t4,t4,...]
    filterGroupedRepeats = True,         -- Reject [t1,t1,t2,t2,t3,t3,t4,t4]
    minRepetitiveLength = 2,             -- More strict: 2+ consecutive repeats
    maxCycleLength = 3                   -- Check smaller cycles only
    }
  }

{-|
Example configuration with filtering disabled for comparison purposes.
Use this when you want the traditional behavior without filtering.
-}
taskWithoutFiltering :: ReachConfig
taskWithoutFiltering = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces = 4,
    numTransitions = 4,
    capacity = Unbounded,
    drawCommands = [Circo],
    maxTransitionLength = 8,
    minTransitionLength = 8,
    postconditionsRange = (2, Just 3),
    preconditionsRange = (2, Just 2)
    },
  printSolution = True,
  rejectLongerThan = Nothing,
  showLengthHint = True,
  showMinLengthHint = True,
  showTargetNet = True,
  showPlaceNamesInNet = True,
  filterConfig = FilterConfig {
    filterCyclicPatterns = False,
    filterRepetitiveSubsequences = False,
    filterGroupedRepeats = False,
    minRepetitiveLength = 3,
    maxCycleLength = 4
    }
  }
