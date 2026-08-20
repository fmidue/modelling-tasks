-- |

module Modelling.ActivityDiagram.FindAuxiliaryPetriNodes.Config where

import Modelling.ActivityDiagram.Common (finalNodesAndTransitionsAdvice)
import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  FindAuxiliaryPetriNodesConfig (..),
  )
import Modelling.ActivityDiagram.Config (AdConfig(..))

{-|
points: 0.15
average generation time per instance: 2:00min
CPU usage: 120%
-}
task2023_41 :: FindAuxiliaryPetriNodesConfig
task2023_41 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 10,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 0
    },
  countOfPetriNodesBounds = (20, Just 26),  -- generates successfully
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.15
average generation time per instance: 3:30min
CPU usage: 100%
-}
task2023_42 :: FindAuxiliaryPetriNodesConfig
task2023_42 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 12,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 0,
    cycles = 2
    },
  countOfPetriNodesBounds = (23, Just 28),  -- fails to generate, but works with (0, Nothing) and (22, Just 40)
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.15
average generation time per instance: 2:24min
CPU usage: 104%
-}
task2024_47 :: FindAuxiliaryPetriNodesConfig
task2024_47 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 10,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 0
    },
  countOfPetriNodesBounds = (23, Just 23),  -- generates successfully; but (0, Just 22) times out with maxInstances = Nothing
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.15
average generation time per instance: 3:51min
CPU usage: 108%
-}
task2024_48 :: FindAuxiliaryPetriNodesConfig
task2024_48 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 12,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 0,
    cycles = 2
    },
  countOfPetriNodesBounds = (33, Just 33),  -- generates successfully; but (0, Just 32) times out with maxInstances = Nothing
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.08
average generation time per instance: 1:51min
CPU usage: 122%
-}
task2024_72 :: FindAuxiliaryPetriNodesConfig
task2024_72 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 10,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 0
    },
  countOfPetriNodesBounds = (21, Just 27),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Just False,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.08
average generation time per instance: 3:28min
CPU usage: 112%
-}
task2024_73 :: FindAuxiliaryPetriNodesConfig
task2024_73 = FindAuxiliaryPetriNodesConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 12,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 1,
    flowFinalNodes = 0,
    cycles = 2
    },
  countOfPetriNodesBounds = (31, Just 41),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Just True,
  printSolution = True,
  extraText = finalNodesAndTransitionsAdvice
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 6:30min
total run time on the cluster (not including queuing time): 6:34min
average CPU usage: 100.80%
average memory usage: 1356.30 MB
used as: FindAuxiliaryPetriNodesUnAvailable-Quiz (at the time)
-}
task2025_50 :: FindAuxiliaryPetriNodesConfig
task2025_50 = FindAuxiliaryPetriNodesConfig {
    adConfig = AdConfig {
      actionLimits = (6, 6),
      objectNodeLimits = (4, 4),
      maxNamedNodes = 10,
      decisionMergePairs = 2,
      forkJoinPairs = 1,
      activityFinalNodes = 0,
      flowFinalNodes = 2,
      cycles = 0
      },
    countOfPetriNodesBounds = (23, Just 24),
    maxInstances = Just 2000,
    hideNodeNames = False,
    hideBranchConditions = True,
    presenceOfSinkTransitionsForFinals = Nothing,
    printSolution = True,
    extraText = finalNodesAndTransitionsAdvice
    }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 6:30min
total run time on the cluster (not including queuing time): 6:35min
average CPU usage: 100.15%
average memory usage: 1291.28 MB
used as: FindAuxiliaryPetriNodesFormInputMultiFieldUnAvailable-Quiz
-}
task2025_repeat_64 :: FindAuxiliaryPetriNodesConfig
task2025_repeat_64 = task2025_50

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 12:11min
total run time on the cluster (not including queuing time): 12:32min
average CPU usage: 100.00%
average memory usage: 1775.43 MB
used as: FindAuxiliaryPetriNodesUnAvailable-Quiz (at the time)
-}
task2025_51 :: FindAuxiliaryPetriNodesConfig
task2025_51 = FindAuxiliaryPetriNodesConfig {
    adConfig = AdConfig {
      actionLimits = (8, 8),
      objectNodeLimits = (4, 4),
      maxNamedNodes = 12,
      decisionMergePairs = 3,
      forkJoinPairs = 1,
      activityFinalNodes = 1,
      flowFinalNodes = 0,
      cycles = 2
      },
    countOfPetriNodesBounds = (33, Just 34),
    maxInstances = Just 2000,
    hideNodeNames = False,
    hideBranchConditions = True,
    presenceOfSinkTransitionsForFinals = Nothing,
    printSolution = True,
    extraText = finalNodesAndTransitionsAdvice
    }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 12:10min
total run time on the cluster (not including queuing time): 12:24min
average CPU usage: 100.00%
average memory usage: 1720.13 MB
used as: FindAuxiliaryPetriNodesFormInputMultiFieldUnAvailable-Quiz
-}
task2025_repeat_65 :: FindAuxiliaryPetriNodesConfig
task2025_repeat_65 = task2025_51

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 6:30min
total run time on the cluster (not including queuing time): 6:37min
average CPU usage: 100.96%
average memory usage: 1290.99 MB
used as: FindAuxiliaryPetriNodesUnAvailable-Quiz (at the time)
-}
task2025_67 :: FindAuxiliaryPetriNodesConfig
task2025_67 = task2025_50

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 6:30min
total run time on the cluster (not including queuing time): 6:38min
average CPU usage: 100.84%
average memory usage: 1290.70 MB
used as: FindAuxiliaryPetriNodesFormInputMultiFieldUnAvailable-Quiz
-}
task2025_repeat_66 :: FindAuxiliaryPetriNodesConfig
task2025_repeat_66 = task2025_50
