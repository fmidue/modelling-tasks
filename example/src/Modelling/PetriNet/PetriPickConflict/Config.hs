-- |

module Modelling.PetriNet.PetriPickConflict.Config where

import Modelling.PetriNet.Types (
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  ConflictConfig (..),
  GraphConfig (..),
  PickConflictConfig (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.1
average generation time per instance: 3:30min
CPU usage: 110%
-}
task2023_22 :: PickConflictConfig
task2023_22 = PickConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 14) ,
    maxTokensPerPlace = 1,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  conflictConfig = ConflictConfig {
    addConflictCommonPreconditions = Just False,
    withConflictDistractors = Just False,
    conflictDistractorAddExtraPreconditions = Nothing,
    conflictDistractorOnlyConflictLike = False,
    conflictDistractorOnlyConcurrentLike = False
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  uniqueConflictPlace = Nothing,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.1
average generation time per instance: 6:00min
CPU usage: 100%
-}
task2023_16 :: PickConflictConfig
task2023_16 = PickConflictConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 2,
    flowOverall = (16, 16) ,
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 1,
    tokensOverall = (5, 5),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 0,
    maxTokenChangePerPlace = 0,
    flowChangeOverall = 2,
    maxFlowChangePerEdge = 1
    },
  conflictConfig = ConflictConfig {
    addConflictCommonPreconditions = Just True,
    withConflictDistractors = Just True,
    conflictDistractorAddExtraPreconditions = Just True,
    conflictDistractorOnlyConflictLike = True,
    conflictDistractorOnlyConcurrentLike = False
    },
  graphConfig = GraphConfig {
    graphLayouts = [Dot, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  uniqueConflictPlace = Just True,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 1000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.1
average generation time per instance: 7:59min
CPU usage: 105%
-}
task2024_30 :: PickConflictConfig
task2024_30 = task2023_16

{-|
points: 0.1
average generation time per instance: 7:01min
CPU usage: 102%
-}
task2024_31 :: PickConflictConfig
task2024_31 = task2023_22

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 58:59min
total run time on the cluster (not including queuing time): 01:04:26h
average CPU usage: 99%
average max memory usage: 1216.20 MB
used in 2025 as: PetriPickConflictRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_32 :: PickConflictConfig
task2025_32 = task2024_30

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 59:02min
total run time on the cluster (not including queuing time): 01:54:06h
average CPU usage: 99%
average max memory usage: 1256.44 MB
used as: PetriPickConflictFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_40 :: PickConflictConfig
task2025_repeat_40 = task2025_32

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 15:11min
total run time on the cluster (not including queuing time): 16:12min
average CPU usage: 100%
average max memory usage: 1375.05 MB
used in 2025 as: PetriPickConflictRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_33 :: PickConflictConfig
task2025_33 = task2024_31

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 15:08min
total run time on the cluster (not including queuing time): 15:50min
average CPU usage: 100%
average max memory usage: 1294.50 MB
used as: PetriPickConflictFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_41 :: PickConflictConfig
task2025_repeat_41 = task2025_33

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 58:40min
total run time on the cluster (not including queuing time): 01:00:51h
average CPU usage: 99%
average max memory usage: 1179.45 MB
used in 2025 as: PetriPickConflictRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_61 :: PickConflictConfig
task2025_61 = task2025_32

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 58:38min
total run time on the cluster (not including queuing time): 01:01:37h
average CPU usage: 99%
average max memory usage: 1179.76 MB
used as: PetriPickConflictFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_42 :: PickConflictConfig
task2025_repeat_42 = task2025_32
