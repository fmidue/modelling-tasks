-- |

module Modelling.ActivityDiagram.SelectPetri.Config where

import qualified Data.Map                         as M

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.SelectPetri (SelectPetriConfig(..))
import Modelling.Auxiliary.Output       (ExtraText(..))
import Control.OutputCapable.Blocks     (Language(..))

import Data.GraphViz.Commands           (GraphvizCommand(..))

-- | Advice text for final nodes in Petri net translation.
-- This text explains how final nodes are realized in Petri nets.
finalNodesAdvice :: ExtraText
finalNodesAdvice = Collapsible True
  (M.fromList [
    (English, "Hint on the translation to a Petri net"),
    (German, "Hinweis zur Übersetzung in ein Petrinetz")
  ])
  (M.fromList [
    (English, "For final nodes no additional places are introduced. They are realised in a way that a token is consumed, i.e. disappears from the net at that position."),
    (German, "Für Endknoten werden keine zusätzlichen Stellen eingeführt. Sie werden so realisiert, dass ein Token verbraucht wird, also an dieser Position aus dem Netz verschwindet.")
  ])

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
  hidePetriNodeLabels = False,
  petriLayout = [Fdp],
  petriSvgHighlighting = True,
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
  petriSvgHighlighting = True,
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
  petriSvgHighlighting = True,
  numberOfWrongAnswers = 5,
  numberOfModifications = 3,
  modifyAtMid = True,
  auxiliaryPetriNodeAbsent = Nothing,
  presenceOfSinkTransitionsForFinals = Nothing,
  withActivityFinalInForkBlocks = Just False,
  printSolution = True,
  extraText = finalNodesAdvice
  }
