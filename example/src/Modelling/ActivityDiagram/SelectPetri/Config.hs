-- |

module Modelling.ActivityDiagram.SelectPetri.Config where

import Modelling.ActivityDiagram.Common (finalNodesAdvice)
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
average generation time per instance: 2:30min
CPU usage: 100%
-}
task2023_37 :: SelectPetriConfig
task2023_37 = SelectPetriConfig {
  adConfig = AdConfig {
    actionLimits = (7, 7),
    objectNodeLimits = (3, 3),
    maxNamedNodes = 10,
    decisionMergePairs = 3,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = False,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
-}
task2023_38 :: SelectPetriConfig
task2023_38 = SelectPetriConfig {
  adConfig = AdConfig {
    actionLimits = (4, 4),
    objectNodeLimits = (3, 3),
    maxNamedNodes = 7,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 1,
  hideNodeNames = True,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Dot],
  petriSvgHighlighting = False,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
average generation time per instance: 3:27min
CPU usage: 110%
-}
task2024_43 :: SelectPetriConfig
task2024_43 = task2023_37

{-|
points: 0.15
average generation time per instance: 1:50min
CPU usage: 114%
-}
task2024_44 :: SelectPetriConfig
task2024_44 = SelectPetriConfig {
  adConfig = AdConfig {
    actionLimits = (4, 4),
    objectNodeLimits = (3, 3),
    maxNamedNodes = 7,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = True,
  hideBranchConditions = True,
  hidePetriNodeLabels = True,
  petriLayout = [Dot],
  petriSvgHighlighting = False,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 8:34min
total run time on the cluster (not including queuing time): 8:48min
average CPU usage: 100%
average memory usage: 1688.99 MB
used as: SelectPetriUnAvailable-Quiz (at the time)
-}
task2025_46 :: SelectPetriConfig
task2025_46 = SelectPetriConfig {
    adConfig = AdConfig {
      actionLimits = (7, 7),
      objectNodeLimits = (3, 3),
      maxNamedNodes = 10,
      decisionMergePairs = 3,
      forkJoinPairs = 1,
      activityFinalNodes = 0,
      flowFinalNodes = 2,
      cycles = 1
      },
    countOfPetriNodesBounds = (26, Just 26),
    maxInstances = Just 2000,
    hideNodeNames = False,
    hideBranchConditions = True,
    hidePetriNodeLabels = True,
    petriLayout = [Fdp],
    petriSvgHighlighting = False,
    numberOfWrongAnswers = 5,
    numberOfModifications = 3,
    modifyAtMid = True,
    auxiliaryPetriNodeAbsent = Nothing,
    presenceOfSinkTransitionsForFinals = Nothing,
    withActivityFinalInForkBlocks = Just False,
    printSolution = True,
    extraText = finalNodesAdvice
    }

{-|
points: 0.125
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 8:38min
total run time on the cluster (not including queuing time): 8:53min
average CPU usage: 100%
average memory usage: 1643.19 MB
used as: SelectPetriFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_58 :: SelectPetriConfig
task2025_repeat_58 = task2025_46

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 3:27min
total run time on the cluster (not including queuing time): 4:24min
average CPU usage: 101.04%
average memory usage: 1172.15 MB
used as: SelectPetriUnAvailable-Quiz (at the time)
-}
task2025_47 :: SelectPetriConfig
task2025_47 = SelectPetriConfig {
    adConfig = AdConfig {
      actionLimits = (4, 4),
      objectNodeLimits = (3, 3),
      maxNamedNodes = 7,
      decisionMergePairs = 2,
      forkJoinPairs = 1,
      activityFinalNodes = 0,
      flowFinalNodes = 2,
      cycles = 1
      },
    countOfPetriNodesBounds = (19, Just 20),
    maxInstances = Just 2000,
    hideNodeNames = True,
    hideBranchConditions = True,
    hidePetriNodeLabels = True,
    petriLayout = [Dot],
    petriSvgHighlighting = False,
    numberOfWrongAnswers = 5,
    numberOfModifications = 3,
    modifyAtMid = True,
    auxiliaryPetriNodeAbsent = Nothing,
    presenceOfSinkTransitionsForFinals = Nothing,
    withActivityFinalInForkBlocks = Just False,
    printSolution = True,
    extraText = finalNodesAdvice
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 3:29min
total run time on the cluster (not including queuing time): 4:09min
average CPU usage: 101.11%
average memory usage: 1319.23 MB
used as: SelectPetriFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_59 :: SelectPetriConfig
task2025_repeat_59 = task2025_47

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 8:39min
total run time on the cluster (not including queuing time): 8:51min
average CPU usage: 100%
average max memory usage: 1644.37 MB
used as: SelectPetriUnAvailable-Quiz (at the time)
-}
task2025_65 :: SelectPetriConfig
task2025_65 = task2025_46

{-|
points: 0.125
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 8:39min
total run time on the cluster (not including queuing time): 8:59min
average CPU usage: 100%
average memory usage: 1643.05 MB
used as: SelectPetriFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_60 :: SelectPetriConfig
task2025_repeat_60 = task2025_46
