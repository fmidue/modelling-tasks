-- |

module Modelling.PetriNet.PetriFindConcurrency.Config where

import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  FindConcurrencyConfig (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
average generation time per instance: 7:00min
CPU usage: 100%
-}
task2023_23 :: FindConcurrencyConfig
task2023_23 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 14),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just True,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 12:10min
CPU usage: 103%
-}
task2024_32 :: FindConcurrencyConfig
task2024_32 = task2023_23

{-|
points: 0.15
average generation time per instance: 26:15min
CPU usage: 106%
-}
task2024_33 :: FindConcurrencyConfig
task2024_33 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (8, 8),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just True,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [TwoPi],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 9:29min
CPU usage: 104%
-}
task2024_62 :: FindConcurrencyConfig
task2024_62 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 2,
    flowOverall = (12, 14),
    maxTokensPerPlace = 0,
    maxFlowPerEdge = 2,
    tokensOverall = (0, 0),
    isConnected = Just True
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 24:39min
CPU usage: 109%
-}
task2024_63 :: FindConcurrencyConfig
task2024_63 = FindConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (14, 16),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (8, 8),
    isConnected = Just False
    },
  advConfig = AdvConfig {
    presenceOfSelfLoops = Just False,
    presenceOfSinkTransitions = Just False,
    presenceOfSourceTransitions = Just False
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [TwoPi],
    hidePlaceNames = True,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }


{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 27:32min
total run time on the cluster (not including queuing time): 28:25min
average CPU usage: 99.85%
average max memory usage: 1542.47 MB
-}
task2025_34 :: FindConcurrencyConfig
task2025_34 = task2024_32

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 1:11:03h
total run time on the cluster (not including queuing time): 2:01:28h
average CPU usage: 99.08%
average max memory usage: 2023.23 MB
-}
task2025_35 :: FindConcurrencyConfig
task2025_35 = task2024_33
