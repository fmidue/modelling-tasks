-- |

module Modelling.ActivityDiagram.MatchPetri.Config where

import qualified Data.Map                         as M

import Modelling.ActivityDiagram.Config (AdConfig(..))
import Modelling.ActivityDiagram.MatchPetri (MatchPetriConfig(..))
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
  countOfPetriNodesBounds = (0, Nothing),
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
  countOfPetriNodesBounds = (0, Nothing),
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
  countOfPetriNodesBounds = (0, Nothing),
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
  countOfPetriNodesBounds = (0, Nothing),
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
