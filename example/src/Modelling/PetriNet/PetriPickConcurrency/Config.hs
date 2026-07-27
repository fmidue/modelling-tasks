-- |

module Modelling.PetriNet.PetriPickConcurrency.Config where

import Modelling.PetriNet.Types (
  AlloyConfig (..),
  BasicConfig (..),
  ChangeConfig (..),
  GraphConfig (..),
  PickConcurrencyConfig (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.GraphViz.Commands           (GraphvizCommand(..))

{-|
points: 0.1
average generation time per instance: 14:00min
CPU usage: 100%
-}
task2023_21 :: PickConcurrencyConfig
task2023_21 = PickConcurrencyConfig {
  basicConfig = BasicConfig {
    places = 6,
    transitions = 6,
    atLeastActive = 5,
    flowOverall = (12, 18),
    maxTokensPerPlace = 2,
    maxFlowPerEdge = 2,
    tokensOverall = (5, 10),
    isConnected = Just True
    },
  changeConfig = ChangeConfig {
    tokenChangeOverall = 4,
    maxTokenChangePerPlace = 2,
    flowChangeOverall = 3,
    maxFlowChangePerEdge = 1
    },
  graphConfig = GraphConfig {
    graphLayouts = [Fdp, Sfdp],
    hidePlaceNames = True,
    hideTransitionNames = True,
    hideWeight1 = True
    },
  printSolution = True,
  prohibitSourceTransitions = False,
  useDifferentGraphLayouts = True,
  alloyConfig = AlloyConfig {
    maxInstances = Just 2000,
    timeout = Nothing
    },
  extraText = NoExtraText
  }

{-|
points: 0.1
average generation time per instance: 15:28min
CPU usage: 102%
-}
task2024_29 :: PickConcurrencyConfig
task2024_29 = task2023_21

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:20:54h
total run time on the cluster (not including queuing time): 04:28:23h
average CPU usage: 99%
average max memory usage: 1234.35 MB
used in 2025 as: PetriPickConcurrencyRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_31 :: PickConcurrencyConfig
task2025_31 = task2024_29

{-|
points: 0.1
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average max memory usage:
used as: PetriPickConcurrencyFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_38 :: PickConcurrencyConfig
task2025_repeat_38 = task2025_31

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:19:38h
total run time on the cluster (not including queuing time): 02:26:58h
average CPU usage: 99%
average max memory usage: 1223.58 MB
used in 2025 as: PetriPickConcurrencyRadioButtonsUnAvailable-Quiz (at the time)
-}
task2025_60 :: PickConcurrencyConfig
task2025_60 = task2025_31

{-|
points: 0.1
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average max memory usage:
used as: PetriPickConcurrencyFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_39 :: PickConcurrencyConfig
task2025_repeat_39 = task2025_31
