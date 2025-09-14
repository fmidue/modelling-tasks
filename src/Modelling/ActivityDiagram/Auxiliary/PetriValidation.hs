{-# LANGUAGE QuasiQuotes #-}
-- | Common validation logic for ActivityDiagram Petri-based tasks
module Modelling.ActivityDiagram.Auxiliary.PetriValidation (
  validatePetriConfig,
  validateSelectPetriSpecific
) where

import qualified Modelling.ActivityDiagram.Config as Config (
  AdConfig (activityFinalNodes, flowFinalNodes, actionLimits, forkJoinPairs, cycles),
  )

import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate (iii)

-- | Common validation logic for configurations that share Petri-related parameters
validatePetriConfig
  :: Config.AdConfig
  -> Maybe Integer  -- maxInstances
  -> [GraphvizCommand]  -- petriLayout
  -> Maybe Bool  -- auxiliaryPetriNodeAbsent
  -> Maybe Bool  -- presenceOfSinkTransitionsForFinals
  -> Maybe Bool  -- withActivityFinalInForkBlocks
  -> Maybe String
validatePetriConfig adConfig maxInstances petriLayout auxiliaryPetriNodeAbsent presenceOfSinkTransitionsForFinals withActivityFinalInForkBlocks
  | Config.activityFinalNodes adConfig > 1
  = Just "There is at most one 'activityFinalNode' allowed."
  | Config.activityFinalNodes adConfig >= 1 && Config.flowFinalNodes adConfig >= 1
  = Just "There is no 'flowFinalNode' allowed if there is an 'activityFinalNode'."
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | auxiliaryPetriNodeAbsent == Just True && Config.cycles adConfig > 0
  = Just [iii|
    Setting the parameter 'auxiliaryPetriNodeAbsent' to True
    prohibits having more than 0 cycles
    |]
  | Just False <- presenceOfSinkTransitionsForFinals,
    fst (Config.actionLimits adConfig) + Config.forkJoinPairs adConfig < 1
    = Just "The option 'presenceOfSinkTransitionsForFinals = Just False' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | withActivityFinalInForkBlocks == Just False && Config.activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'withActivityFinalInForkBlocks' to False prohibits having more than 1 'activityFinalNodes'"
  | withActivityFinalInForkBlocks == Just True && Config.activityFinalNodes adConfig == 0
    = Just "Setting the parameter 'withActivityFinalInForkBlocks' to True implies that there are 'activityFinalNodes'"
  | null petriLayout
    = Just "The parameter 'petriLayout' can not be the empty list"
  | any (`notElem` [Dot, Neato, TwoPi, Circo, Fdp]) petriLayout
    = Just "The parameter 'petriLayout' can only contain the options Dot, Neato, TwoPi, Circo and Fdp"
  | otherwise
    = Nothing

-- | Additional validation specific to SelectPetri configurations
validateSelectPetriSpecific
  :: Int  -- numberOfWrongAnswers
  -> Int  -- numberOfModifications
  -> Maybe String
validateSelectPetriSpecific numberOfWrongAnswers numberOfModifications
  | numberOfWrongAnswers < 1
    = Just "The parameter 'numberOfWrongAnswers' must be set to a positive value"
  | numberOfModifications < 1
    = Just "The parameter 'numberOfModifications' must be set to a positive value"
  | otherwise
    = Nothing