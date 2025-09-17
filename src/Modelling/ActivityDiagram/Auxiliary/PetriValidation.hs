{-# LANGUAGE QuasiQuotes #-}
-- | Common validation logic for ActivityDiagram Petri-based tasks
module Modelling.ActivityDiagram.Auxiliary.PetriValidation (
  validatePetriConfig,
  validateBasePetriConfig
) where

import qualified Modelling.ActivityDiagram.Config as Config (
  AdConfig (activityFinalNodes, flowFinalNodes, actionLimits, objectNodeLimits, forkJoinPairs, decisionMergePairs, cycles),
  )

import Control.Applicative (Alternative ((<|>)))
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate (iii)

-- | Calculate minimum number of Petri net nodes based on AdConfig values
calculateMinimumPetriNodes :: Config.AdConfig -> Int
calculateMinimumPetriNodes adConfig =
  let
    -- At least one initial node (always 1 place)
    initialNodes = 1
    
    -- Minimum action nodes (each action becomes a transition, plus places before/after)
    minActionNodes = fst (Config.actionLimits adConfig)
    
    -- Minimum object nodes (each becomes a place)
    minObjectNodes = fst (Config.objectNodeLimits adConfig)
    
    -- Final nodes (each becomes a place)
    finalNodes = Config.activityFinalNodes adConfig + Config.flowFinalNodes adConfig
    
    -- Fork/Join pairs (each pair adds auxiliary nodes: roughly 2 nodes per pair)
    forkJoinNodes = Config.forkJoinPairs adConfig * 2
    
    -- Decision/Merge pairs (each pair adds auxiliary nodes: roughly 2 nodes per pair)  
    decisionMergeNodes = Config.decisionMergePairs adConfig * 2
    
  in initialNodes + minActionNodes + minObjectNodes + finalNodes + forkJoinNodes + decisionMergeNodes

-- | Base validation logic common to multiple Petri-based configurations
validateBasePetriConfig
  :: Config.AdConfig
  -> (Int, Maybe Int)  -- countOfPetriNodesBounds
  -> Maybe Integer  -- maxInstances
  -> Maybe Bool  -- presenceOfSinkTransitionsForFinals
  -> Maybe String
validateBasePetriConfig adConfig countOfPetriNodesBounds maxInstances presenceOfSinkTransitionsForFinals
  | Config.activityFinalNodes adConfig > 1
  = Just "There is at most one 'activityFinalNode' allowed."
  | Config.activityFinalNodes adConfig >= 1 && Config.flowFinalNodes adConfig >= 1
  = Just "There is no 'flowFinalNode' allowed if there is an 'activityFinalNode'."
  | fst countOfPetriNodesBounds < 0
  = Just "'countOfPetriNodesBounds' must not contain negative values"
  | Just high <- snd countOfPetriNodesBounds, fst countOfPetriNodesBounds > high
  = Just "the second value of 'countOfPetriNodesBounds' must not be smaller than its first value"
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | Just False <- presenceOfSinkTransitionsForFinals,
    fst (Config.actionLimits adConfig) + Config.forkJoinPairs adConfig < 1
    = Just "The option 'presenceOfSinkTransitionsForFinals = Just False' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | fst countOfPetriNodesBounds > 0 && fst countOfPetriNodesBounds < calculateMinimumPetriNodes adConfig
    = Just [iii|
      The minimum value of 'countOfPetriNodesBounds' (${fst countOfPetriNodesBounds}) is too small. 
      Based on the AdConfig values, the minimum number of Petri net nodes should be at least ${calculateMinimumPetriNodes adConfig}.
      This is calculated from: 1 initial node + ${fst (Config.actionLimits adConfig)} minimum action nodes + ${fst (Config.objectNodeLimits adConfig)} minimum object nodes + ${Config.activityFinalNodes adConfig + Config.flowFinalNodes adConfig} final nodes + ${Config.forkJoinPairs adConfig * 2} fork/join auxiliary nodes + ${Config.decisionMergePairs adConfig * 2} decision/merge auxiliary nodes.
      |]
  | otherwise
    = Nothing

-- | Common validation logic for configurations that share Petri-related parameters
validatePetriConfig
  :: Config.AdConfig
  -> (Int, Maybe Int)  -- countOfPetriNodesBounds
  -> Maybe Integer  -- maxInstances
  -> [GraphvizCommand]  -- petriLayout
  -> Maybe Bool  -- auxiliaryPetriNodeAbsent
  -> Maybe Bool  -- presenceOfSinkTransitionsForFinals
  -> Maybe Bool  -- withActivityFinalInForkBlocks
  -> Maybe String
validatePetriConfig
  adConfig
  countOfPetriNodesBounds
  maxInstances
  petriLayout
  auxiliaryPetriNodeAbsent
  presenceOfSinkTransitionsForFinals
  withActivityFinalInForkBlocks =
  validateBasePetriConfig adConfig countOfPetriNodesBounds maxInstances presenceOfSinkTransitionsForFinals
  <|> validatePetriConfigSpecific
  where
    validatePetriConfigSpecific
      | auxiliaryPetriNodeAbsent == Just True && Config.cycles adConfig > 0
      = Just [iii|
        Setting the parameter 'auxiliaryPetriNodeAbsent' to True
        prohibits having more than 0 cycles
        |]
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
