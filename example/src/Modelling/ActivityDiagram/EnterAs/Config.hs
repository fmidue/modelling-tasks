-- |

module Modelling.ActivityDiagram.EnterAs.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.EnterAS (EnterASConfig(..))

import Control.OutputCapable.Blocks     (ExtraText (..))

{-|
points: 0.15
average generation time per instance: 3:30min
CPU usage: 100%
-}
task2023_35 :: EnterASConfig
task2023_35 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (12, 12),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 17,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just True,
  answerLength = (10, 10),
  rejectLongerThan = Nothing,
  printSolution = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 5:00min
CPU usage: 100%
-}
task2023_36 :: EnterASConfig
task2023_36 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (14, 14),
    objectNodeLimits = (6, 6),
    maxNamedNodes = 20,
    decisionMergePairs = 1,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just False,
  answerLength = (14, 14),
  rejectLongerThan = Nothing,
  printSolution = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 3:57min
CPU usage: 109%
-}
task2024_41 :: EnterASConfig
task2024_41 = EnterASConfig {
  adConfig = AdConfig {
    actionLimits = (12, 12),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 17,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 2
    },
  hideBranchConditions = True,
  maxInstances = Just 2000,
  objectNodeOnEveryPath = Just True,
  answerLength = (11, 11),
  rejectLongerThan = Nothing,
  printSolution = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 3:23min
CPU usage: 113%
-}
task2024_42 :: EnterASConfig
task2024_42 = task2023_36

{-|
points: 0.08
average generation time per instance: 6:11min
CPU usage: 107%
-}
task2024_68 :: EnterASConfig
task2024_68 = task2024_41

{-|
points: 0.08
average generation time per instance: 5:16min
CPU usage: 109%
-}
task2024_69 :: EnterASConfig
task2024_69 = task2023_36

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 19:11min
total run time on the cluster (not including queuing time): 19:42min
average CPU usage: 100%
average memory usage: 2297.78 MB
used as: EnterASUnAvailable-Quiz (at the time)
-}
task2025_44 :: EnterASConfig
task2025_44 = task2024_41

{-|
points: 0.15
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: EnterASFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_55 :: EnterASConfig
task2025_repeat_55 = task2025_44

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 18:08min
total run time on the cluster (not including queuing time): 19:10min
average CPU usage: 100%
average memory usage: 2273.13 MB
used as: EnterASUnAvailable-Quiz (at the time)
-}
task2025_45 :: EnterASConfig
task2025_45 = task2024_42

{-|
points: 0.125
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: EnterASFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_56 :: EnterASConfig
task2025_repeat_56 = task2025_45

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 18:06min
total run time on the cluster (not including queuing time): 18:29min
average CPU usage: 100%
average memory usage: 2278.10 MB
used as: EnterASUnAvailable-Quiz (at the time)
-}
task2025_64 :: EnterASConfig
task2025_64 = task2025_45

{-|
points: 0.125
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: EnterASFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_57 :: EnterASConfig
task2025_repeat_57 = task2025_45
