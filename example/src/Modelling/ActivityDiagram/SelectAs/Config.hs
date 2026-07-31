-- |

module Modelling.ActivityDiagram.SelectAs.Config where
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..))

import Control.OutputCapable.Blocks     (ExtraText (..))

{-|
points: 0.15
average generation time per instance: 22:00min
CPU usage: 100%
-}
task2023_33 :: SelectASConfig
task2023_33 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (10, 10),
    objectNodeLimits = (2, 2),
    maxNamedNodes = 12,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 2
    },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Just True,
  numberOfWrongAnswers = 6,
  answerLength = (10, 10),
  printSolution = True,
  withActionRepetition = False,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 22:00min
CPU usage: 100%
-}
task2023_34 :: SelectASConfig
task2023_34 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (10, 10),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 15,
    decisionMergePairs = 2,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 9,
  answerLength = (9, 9),
  printSolution = True,
  withActionRepetition = False,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 25:33min
CPU usage: 101%
-}
task2024_39 :: SelectASConfig
task2024_39 = task2023_33

{-|
points: 0.15
average generation time per instance: 24:09min
CPU usage: 101%
-}
task2024_40 :: SelectASConfig
task2024_40 = task2023_34

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 25:43min
total run time on the cluster (not including queuing time): 26:13min
average CPU usage: 99.12%
average memory usage: 1744.60 MB
used as: SelectASUnAvailable-Quiz (at the time)
-}
task2025_41 :: SelectASConfig
task2025_41 = task2024_39

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 25:53min
total run time on the cluster (not including queuing time): 27:09min
average CPU usage: 99.71%
average memory usage: 1667.42 MB
used as: SelectASFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_51 :: SelectASConfig
task2025_repeat_51 = task2025_41

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 30:55min
total run time on the cluster (not including queuing time): 37:46min
average CPU usage: 99%
average memory usage: 11696.96 MB
used as: SelectASUnAvailable-Quiz (at the time)
-}
task2025_42 :: SelectASConfig
task2025_42 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (10, 10),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 15,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 1,
    cycles = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 9,
  answerLength = (11, 11),
  printSolution = True,
  withActionRepetition = True,
  extraText = NoExtraText
}

{-|
points: 0.125
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 31:48min
total run time on the cluster (not including queuing time): 39:29min
average CPU usage: 99.40%
average memory usage: 11572.85 MB
used as: SelectASFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_52 :: SelectASConfig
task2025_repeat_52 = task2025_42

{-|
points: 0.15
the amount of generated instances: 50
maximum concurrent amount of tasks: 1 (Out-of-memory errors sometimes occur, which can cause parallel programs to terminate)
average generation time per instance on the cluster (without considering concurrency): about 7:17min (if successful generation)
used as: SelectASUnAvailable-Quiz (at the time)
-}
task2025_43 :: SelectASConfig
task2025_43 = SelectASConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (0, 0),
    maxNamedNodes = 8,
    decisionMergePairs = 2,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 500,
  objectNodeOnEveryPath = Just False,
  numberOfWrongAnswers = 9,
  answerLength = (11, 11),
  printSolution = True,
  withActionRepetition = True,
  extraText = NoExtraText
}

{-|
points: 0.15
use same instances as with task2025_43 (suffer from same issue as task2025_43)
used as: SelectASFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_53 :: SelectASConfig
task2025_repeat_53 = task2025_43

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 32:17min
total run time on the cluster (not including queuing time): 39:31min
average CPU usage: 99.03%
average memory usage: 11647.41 MB
used as: SelectASUnAvailable-Quiz (at the time)
-}
task2025_63 :: SelectASConfig
task2025_63 = task2025_42

{-|
points: 0.125
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 30:23min
total run time on the cluster (not including queuing time): 40:03min
average CPU usage: 99.01%
average memory usage: 11594.96 MB
used as: SelectASFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_54 :: SelectASConfig
task2025_repeat_54 = task2025_42
