-- |

module Modelling.ActivityDiagram.FindAuxiliaryPetriNodes.Config where

import qualified Data.Map                         as M

import Modelling.ActivityDiagram.FindAuxiliaryPetriNodes (
  FindAuxiliaryPetriNodesConfig (..),
  )
import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.Auxiliary.Output       (ExtraText(..))
import Control.OutputCapable.Blocks     (Language(..))

-- | Extended advice text for final nodes and auxiliary transitions.
-- This text explains that transitions required for realizing final node behavior
-- do not count as auxiliary nodes.
finalNodeTransitionAdvice :: ExtraText
finalNodeTransitionAdvice = Collapsible True
  (M.fromList [
    (English, "Additional information in the context of final nodes"),
    (German, "Zusätzliche Informationen im Kontext von Endknoten")
  ])
  (M.fromList [
    (English, "As mentioned, final nodes are realised by letting a token disappear. If an additional transition is required to realise this behavior, this transition does not count as auxiliary node."),
    (German, "Wie angemerkt, werden Endknoten so realisiert, dass ein Token verschwinden gelassen wird. Falls eine zusätzliche Transition erforderlich ist, um dieses Verhalten zu realisieren, zählt diese Transition nicht als Hilfsknoten.")
  ])

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
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodeTransitionAdvice
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
  countOfPetriNodesBounds = (0, Nothing),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodeTransitionAdvice
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
  countOfPetriNodesBounds = (21, Just 27),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodeTransitionAdvice
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
  countOfPetriNodesBounds = (31, Just 41),
  maxInstances = Just 2000,
  hideNodeNames = False,
  hideBranchConditions = True,
  presenceOfSinkTransitionsForFinals = Nothing,
  printSolution = True,
  extraText = finalNodeTransitionAdvice
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
  extraText = finalNodeTransitionAdvice
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
  extraText = finalNodeTransitionAdvice
  }
