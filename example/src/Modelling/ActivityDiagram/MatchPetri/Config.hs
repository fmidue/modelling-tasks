-- |

module Modelling.ActivityDiagram.MatchPetri.Config where

import Modelling.ActivityDiagram.Common (finalNodesAdvice)
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchPetri (MatchPetriConfig(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
average generation time per instance: 40:00min
CPU usage: 100%
-}
task2023_39 :: MatchPetriConfig
task2023_39 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (4, 4),
    maxNamedNodes = 12,
    decisionMergePairs = 2,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 2,
    cycles = 1
    },
  countOfPetriNodesBounds = (25, Just 25),  -- generates successfully, but smaller upper bound leads to NoInstanceAvailable
  maxInstances = Just 10000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Nothing,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
average generation time per instance: 40:00min
CPU usage: 100%
-}
task2023_40 :: MatchPetriConfig
task2023_40 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 13,
    decisionMergePairs = 3,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 3,
    cycles = 3
    },
  countOfPetriNodesBounds = (28, Just 34),  -- fails to generate, but works with (0, Nothing) and (28, Just 46) as well as (37, Just 37); upper bound smaller than 37 leads to NoInstanceAvailable
  maxInstances = Just 2000, -- fails to generate with Nothing, it diverges
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Nothing,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
average generation time per instance: 23:20min
CPU usage: 103%
-}
task2024_45 :: MatchPetriConfig
task2024_45 = task2023_39

{-|
points: 0.15
average generation time per instance: 1:06:10h
CPU usage: 102%
-}
task2024_46 :: MatchPetriConfig
task2024_46 = task2023_40

{-|
points: 0.08
average generation time per instance: 18:16min
CPU usage: 105%
-}
task2024_70 :: MatchPetriConfig
task2024_70 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (7, 7),
    maxNamedNodes = 13,
    decisionMergePairs = 1,
    forkJoinPairs = 1,
    activityFinalNodes = 0,
    flowFinalNodes = 1,
    cycles = 0
    },
  countOfPetriNodesBounds = (20, Just 30),  -- fails to generate, even with (0, Nothing): NoInstanceAvailable
  maxInstances = Just 10000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Just True,
  presenceOfSinkTransitionsForFinals = Just True,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.08
average generation time per instance: 14:13min
CPU usage: 104%
-}
task2024_71 :: MatchPetriConfig
task2024_71 = MatchPetriConfig {
  adConfig = AdConfig {
    actionLimits = (8, 8),
    objectNodeLimits = (5, 5),
    maxNamedNodes = 13,
    decisionMergePairs = 3,
    forkJoinPairs = 2,
    activityFinalNodes = 0,
    flowFinalNodes = 3,
    cycles = 3
    },
  countOfPetriNodesBounds = (28, Just 34),  -- fails to generate: NoInstanceAvailable; diverges when (0, Nothing)
  maxInstances = Just 2000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Just False,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 39:44min
total run time on the cluster (not including queuing time): 40:26min
average CPU usage: 100%
average memory usage: 9190.32 MB
-}
task2025_48 :: MatchPetriConfig
task2025_48 = MatchPetriConfig {
    adConfig = AdConfig {
      actionLimits = (8, 8),
      objectNodeLimits = (4, 4),
      maxNamedNodes = 12,
      decisionMergePairs = 2,
      forkJoinPairs = 1,
      activityFinalNodes = 0,
      flowFinalNodes = 2,
      cycles = 1
      },
    countOfPetriNodesBounds = (25, Just 26),
    maxInstances = Just 10000,
    hideBranchConditions = True,
    petriLayout = [Fdp],
    petriSvgHighlighting = True,
    auxiliaryPetriNodeAbsent = Nothing,
    presenceOfSinkTransitionsForFinals = Nothing,
    withActivityFinalInForkBlocks = Nothing,
    printSolution = True,
    extraText = finalNodesAdvice
    }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 41:20min
total run time on the cluster (not including queuing time): 49:04min
average CPU usage: 99.24%
average memory usage: 2268.47 MB
-}
task2025_49 :: MatchPetriConfig
task2025_49 = MatchPetriConfig {
    adConfig = AdConfig {
      actionLimits = (8, 8),
      objectNodeLimits = (5, 5),
      maxNamedNodes = 13,
      decisionMergePairs = 3,
      forkJoinPairs = 2,
      activityFinalNodes = 0,
      flowFinalNodes = 3,
      cycles = 3
      },
    countOfPetriNodesBounds = (37, Just 38),
    maxInstances = Just 2000,
    hideBranchConditions = True,
    petriLayout = [Fdp],
    petriSvgHighlighting = True,
    auxiliaryPetriNodeAbsent = Nothing,
    presenceOfSinkTransitionsForFinals = Nothing,
    withActivityFinalInForkBlocks = Nothing,
    printSolution = True,
    extraText = finalNodesAdvice
    }
