-- |

module Modelling.ActivityDiagram.MatchPetri.Config where

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
  maxInstances = Just 10000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Nothing,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Nothing,
  printSolution = True
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
  maxInstances = Just 2000,
  hideBranchConditions = True,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
  auxiliaryPetriNodeAbsent = Nothing,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Nothing,
  printSolution = True
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
