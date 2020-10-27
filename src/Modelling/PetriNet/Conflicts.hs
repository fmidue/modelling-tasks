{-# LANGUAGE NamedFieldPuns #-}
{-# Language DuplicateRecordFields #-}

module Modelling.PetriNet.Conflicts (
  findConflicts, findConflictsTaskInstance, findConflictsTask,
  getAlloyInstances,
  pickConflicts, pickConflictsTaskInstance, pickConflictsTask,
  checkFindConflictConfig, checkPickConflictConfig
  ) where

import Modelling.PetriNet.Alloy (
  getAlloyInstances, petriNetFindConfl, petriNetPickConfl,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram       (getNetWith)
import Modelling.PetriNet.Parser        (
  parseConflict
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), Conflict, FindConflictConfig(..), PickConflictConfig(..),
  )

import Control.Monad                    (unless)
import Control.Monad.Trans.Except       (ExceptT, except)
import Data.GraphViz.Attributes.Complete (GraphvizCommand)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                  (Diagram)
import Language.Alloy.Call (
  AlloyInstance, CallAlloyConfig (..), defaultCallAlloyConfig,
  )

findConflicts
  :: Int
  -> FindConflictConfig
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflicts indInst config@FindConflictConfig{basicConfig} = do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetFindConfl config)
  unless (length list > indInst) $ except $ Left "instance not available"
  findConflictsTaskInstance (list !! indInst) (graphLayout basicConfig)

findConflictsTask :: String
findConflictsTask =
  "Which of the following Petrinets doesn't have a conflict?"

pickConflicts
  :: Int
  -> PickConflictConfig
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflicts indInst  config@PickConflictConfig{basicConfig}= do
  list <- getAlloyInstances
    defaultCallAlloyConfig {
      maxInstances = Just $ toInteger $ indInst + 1
      }
    (petriNetPickConfl config)
  unless (length list > indInst) $ except $ Left "instance not available"
  pickConflictsTaskInstance (list !! indInst) (graphLayout basicConfig)

findConflictsTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe Conflict)
findConflictsTaskInstance = getNet "flow" "tokens"

pickConflictsTaskInstance
  :: AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO [(Diagram B, Maybe Conflict)]
pickConflictsTaskInstance inst gc = do
  confl <- getNet "flow" "tokens" inst gc
  net   <- getNet "defaultFlow" "defaultTokens" inst gc
  return [confl,net]

pickConflictsTask :: String
pickConflictsTask =
  "Which pair of transitions are in conflict under the initial marking?"

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig

getNet
  :: String
  -> String
  -> AlloyInstance
  -> GraphvizCommand
  -> ExceptT String IO (Diagram B, Maybe Conflict)
getNet = getNetWith parseConflict
