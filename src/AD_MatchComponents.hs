{-# LANGUAGE NamedFieldPuns #-}

module AD_MatchComponents (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  checkMatchPetriConfig,
  matchPetriComponents
) where

import qualified Data.Map as M ((!), insert, delete, keys, empty, null)

import qualified AD_Datatype as AD (
  UMLActivityDiagram(..),
  ADNode(..),
  isActionNode, isObjectNode, isDecisionNode, isMergeNode, isForkNode, isJoinNode, isInitialNode, isActivityFinalNode, isFlowFinalNode)

import AD_Petrinet (PetriKey(..))
import AD_Config (ADConfig(..), checkADConfig)

import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Control.Applicative (Alternative ((<|>)))
import Data.Map (Map)

data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: AD.UMLActivityDiagram,
  petrinet :: PetriLike PetriKey            -- Is this needed or should be generated from ad here?
}

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: ADConfig,
  mustHaveSomeSupportSTs :: Maybe Bool,    -- Option to force support STs to occur
  allowActivityFinals :: Maybe Bool,      -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool -- Avoid having to add new sink transitions for representing finals
} deriving (Show)


checkMatchPetriConfig :: MatchPetriConfig -> Maybe String 
checkMatchPetriConfig conf =
  checkADConfig (adConfig conf) 
  <|> checkMatchPetriConfig' conf


checkMatchPetriConfig' :: MatchPetriConfig -> Maybe String 
checkMatchPetriConfig' MatchPetriConfig {
    adConfig,    
    allowActivityFinals,      
    avoidAddingSinksForFinals
  }
  | allowActivityFinals == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'allowActivityFinals' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig <= 0
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise 
    = Nothing

mapTypesToLabels :: AD.UMLActivityDiagram -> Map String [Int]
mapTypesToLabels diag =
  let actionLabels = extractLabels AD.isActionNode  
      objectLabels = extractLabels AD.isObjectNode 
      decisionLabels = extractLabels AD.isDecisionNode 
      mergeLabels = extractLabels AD.isMergeNode 
      forkLabels = extractLabels AD.isForkNode
      joinLabels = extractLabels AD.isJoinNode 
      initialLabels = extractLabels AD.isInitialNode 
      activtiyFinalLabels = extractLabels AD.isActivityFinalNode 
      flowFinalLabels = extractLabels AD.isFlowFinalNode 
  in M.insert "ActionNodes" actionLabels $
     M.insert "ObjectNodes" objectLabels $
     M.insert "DecisionNodes" decisionLabels $
     M.insert "MergeNodes" mergeLabels $
     M.insert "ForkNodes" forkLabels $
     M.insert "JoinNodes" joinLabels $
     M.insert "InitialNodes" initialLabels $
     M.insert "ActivityFinalNodes" activtiyFinalLabels $
     M.insert "FlowFinalNodes" flowFinalLabels 
     M.empty
  where extractLabels fn = map AD.label $ filter fn $ AD.nodes diag

--Precondition: petri was generated from diag via convertToPetrinet
matchPetriComponents :: AD.UMLActivityDiagram -> PetriLike PetriKey -> Map String [Int]
matchPetriComponents diag petri =
  let labelMap = M.delete "FlowFinalNodes" $ M.delete "ActivityFinalNodes" $ mapTypesToLabels diag
      supportST = map label $ filter (\x -> isSupportST x && not (isSinkST x petri)) $ M.keys $ allNodes petri
  in M.insert "SupportST" supportST labelMap

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False