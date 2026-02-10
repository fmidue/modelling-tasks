-- |

module Modelling.ActivityDiagram.MatchAd.Config where

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchAd (MatchAdConfig(..))

import Control.OutputCapable.Blocks     (ExtraText (..))

{-|
points: 0.15
-}
task2023_31 :: MatchAdConfig
task2023_31 = MatchAdConfig {
  adConfig = AdConfig {
    actionLimits = (5, 6),
    objectNodeLimits = (5, 6),
    maxNamedNodes = 11,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 1,
    cycles = 1
    },
  maxInstances = Just 500,
  hideBranchConditions = False,
  withActivityFinalInForkBlocks = Just True,
  printSolution = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
-}
task2023_32 :: MatchAdConfig
task2023_32 = MatchAdConfig {
  adConfig = AdConfig {
    actionLimits = (5, 6),
    objectNodeLimits = (5, 6),
    maxNamedNodes = 11,
    decisionMergePairs = 1,
    forkJoinPairs = 2,
    activityFinalNodes = 1,
    flowFinalNodes = 2,
    cycles = 1
    },
  maxInstances = Just 500,
  hideBranchConditions = True,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 0:28min
CPU usage: 172%
-}
task2024_37 :: MatchAdConfig
task2024_37 = task2023_31

{-|
points: 0.15
average generation time per instance: 0:35min
CPU usage: 158%
-}
task2024_38 :: MatchAdConfig
task2024_38 = task2023_32

{-|
points: 0.08
average generation time per instance: 0:38min
CPU usage: 156%
-}
task2024_67 :: MatchAdConfig
task2024_67 = task2023_31

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:16min
total run time on the cluster (not including queuing time): 2:20min
average CPU usage: 101.93%
average memory usage: 408.60 MB
-}
task2025_39 :: MatchAdConfig
task2025_39 = task2024_37


{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 3:22min
total run time on the cluster (not including queuing time): 3:27min
average CPU usage: 100.80%
average memory usage: 455.67 MB
-}
task2025_40 :: MatchAdConfig
task2025_40 = task2024_38
