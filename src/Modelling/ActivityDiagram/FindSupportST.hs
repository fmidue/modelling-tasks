{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Modelling.ActivityDiagram.FindSupportST (
  FindSupportSTInstance(..),
  FindSupportSTConfig(..),
  defaultFindSupportSTConfig,
  checkFindSupportSTConfig,
  findSupportST,
  findSupportSTAlloy,
  findSupportSTTaskDescription,
  findSupportSTText,
) where

import qualified Data.Map as M ((!), null, size, filter, filterWithKey)

import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)

import Modelling.PetriNet.Types (PetriLike(..), Node(..), isPlaceNode, isTransitionNode)

import Control.Applicative (Alternative ((<|>)))
import Data.String.Interpolate ( i )


data FindSupportSTInstance = FindSupportSTInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int
} deriving (Show)

data FindSupportSTConfig = FindSupportSTConfig {
  adConfig :: ADConfig,
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool   -- Avoid having to add new sink transitions for representing finals
} deriving (Show)

defaultFindSupportSTConfig :: FindSupportSTConfig
defaultFindSupportSTConfig = FindSupportSTConfig
  { adConfig = defaultADConfig,
    activityFinalsExist = Nothing,
    avoidAddingSinksForFinals = Nothing
  }

checkFindSupportSTConfig :: FindSupportSTConfig -> Maybe String
checkFindSupportSTConfig conf =
  checkADConfig (adConfig conf)
  <|> findSupportSTConfig' conf

findSupportSTConfig' :: FindSupportSTConfig -> Maybe String
findSupportSTConfig' FindSupportSTConfig {
    adConfig,
    activityFinalsExist,
    avoidAddingSinksForFinals
  }
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | otherwise
    = Nothing

findSupportSTAlloy :: FindSupportSTConfig -> String
findSupportSTAlloy FindSupportSTConfig {
  adConfig,
  activityFinalsExist,
  avoidAddingSinksForFinals
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            not supportSTAbsent
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

findSupportSTTaskDescription :: String
findSupportSTTaskDescription =
  [i|
    Look at the given Activity Diagram and convert it to a petri net by hand,
    then:

    a) Enter the number of nodes of the resulting petri net
    b) Enter the number of support places of the resulting petri net
    c) Enter the number of support transitions of the resulting petri net
  |]

findSupportSTText :: FindSupportSTInstance -> (UMLActivityDiagram, String)
findSupportSTText inst =
  let (ad, solution) = findSupportST inst
      text = [i|
      Solutions for the FindSupportST-Task:

      a) Number of nodes of the resulting petri net: #{numberOfPetriNodes solution}
      b) Number of support places in the resulting petri net: #{numberOfSupportPlaces solution}
      c) Number of support transitions in the resulting petri net: #{numberOfSupportTransitions solution}
      |]
  in (ad, text)

data FindSupportSTSolution = FindSupportSTSolution {
  numberOfPetriNodes :: Int,
  numberOfSupportPlaces :: Int,
  numberOfSupportTransitions :: Int
} deriving (Show, Eq)

findSupportST :: FindSupportSTInstance -> (UMLActivityDiagram, FindSupportSTSolution)
findSupportST FindSupportSTInstance {
  activityDiagram,
  seed
} =
  let ad =  snd $ shuffleADNames seed activityDiagram
      solution = findSupportST' $ convertToPetrinet ad
  in (ad, solution)

findSupportST' :: PetriLike PetriKey -> FindSupportSTSolution
findSupportST' petri = FindSupportSTSolution {
    numberOfPetriNodes = M.size $ allNodes petri,
    numberOfSupportPlaces = M.size $ M.filter isPlaceNode supportSTMap,
    numberOfSupportTransitions = M.size $ M.filter isTransitionNode supportSTMap
  }
  where
    supportSTMap = M.filterWithKey (\k _ -> isSupportST k && not (isSinkST k petri)) $ allNodes petri

isSinkST :: PetriKey -> PetriLike PetriKey -> Bool
isSinkST key petri = M.null $ flowOut $ allNodes petri M.! key

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False