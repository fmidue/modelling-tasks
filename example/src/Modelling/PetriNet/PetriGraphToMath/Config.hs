{-|
Configurations might work for @PetriGraphToMath@ and @PetriMathToGraph@ tasks
-}
module Modelling.PetriNet.PetriGraphToMath.Config where

import Modelling.PetriNet.MatchToMath (
  MathConfig (..),
  )
import Modelling.PetriNet.Types (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.15
average generation time per instance: 4:00min
CPU usage: 110%
-}
task2023_17 :: MathConfig
task2023_17 = MathConfig {
  basicConfig = BasicConfig {
    places = 5,
    transitions = 7,
    atLeastActive = 3,
    flowOverall = (13, 15),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (10, 10),
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
  generatedWrongInstances = 300,
  graphConfig = GraphConfig {
    graphLayouts = [Sfdp],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  useDifferentGraphLayouts = False,
  wrongInstances = 3,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 6:00min
CPU usage: 110%
-}
task2023_18 :: MathConfig
task2023_18 = MathConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 5,
    atLeastActive = 2,
    flowOverall = (15, 17),
    maxTokensPerPlace = 1,
    maxFlowPerEdge = 1,
    tokensOverall = (5, 5),
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
  generatedWrongInstances = 300,
  graphConfig = GraphConfig {
    graphLayouts = [Fdp],
    hidePlaceNames = False,
    hideTransitionNames = False,
    hideWeight1 = True
    },
  printSolution = True,
  useDifferentGraphLayouts = False,
  wrongInstances = 3 ,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.1
average generation time per instance: 5:16min
CPU usage: 105%
-}
task2024_21 :: MathConfig
task2024_21 = task2023_17

{-|
points: 0.1
average generation time per instance: 10:28min
CPU usage: 103%
-}
task2024_22 :: MathConfig
task2024_22 = task2023_18

{-|
points: 0.05
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 12:48min
total run time on the cluster (not including queuing time): 26:50min
average CPU usage: 101%
average max memory usage: 986.73 MB
Note: 1 student task is rendered faulty on Autotool due to the different graphviz version (manually fixed).
used in 2025 as: PetriGraphToMathRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_23 :: MathConfig
task2025_23 = task2024_21

{-|
points: 0.05
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 12:09min
total run time on the cluster (not including queuing time): 12:38min
average CPU usage: 100%
average max memory usage: 983.64 MB
used as: PetriGraphToMathFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_28 :: MathConfig
task2025_repeat_28 = task2025_23

{-|
points: 0.05
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 32:20min
total run time on the cluster (not including queuing time): 01:51:17h
average CPU usage: 100%
average max memory usage: 1255.89 MB
Note: about 15 student tasks are rendered faulty on Autotool due to the different graphviz version (manually fixed).
used in 2025 as: PetriGraphToMathRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_24 :: MathConfig
task2025_24 = task2024_22

{-|
points: 0.05
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 31:31min
total run time on the cluster (not including queuing time): 1:51:31h
average CPU usage: 100%
average max memory usage: 1004.07 MB
used as: PetriGraphToMathFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_29 :: MathConfig
task2025_repeat_29 = task2025_24
