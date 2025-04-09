{-# Language DuplicateRecordFields #-}
{-# Language RecordWildCards #-}

module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Common (
  forceErrors,
  instanceInput,
  withLang,
  )
import Modelling.PetriNet.Capacity (
  checkCapacityConfigs,
  capacityGenerate,
  simpleCapacityTask,
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..),
  CapacityConfig(..),
  defaultCapacityConfig,
  )

import Control.OutputCapable.Blocks     (Language (English))
import Control.Monad.Trans.Class        (lift)
import Data.Maybe                       (isNothing)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)
import Text.Read                         (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Generating instance for converting nets with capacities into nets without capacities"
  i <- instanceInput
  if i >= 0
  then mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  let theConfig@CapacityConfig{..} = defaultCapacityConfig
  lift $ pPrint theConfig
  (pls, trns, maxCap, newFlowMin, newFlowMax, oneMin, distractMin, distractMax, atMostAct) <- lift $ userInput theConfig
  let config = theConfig {
        basicConfig = basicConfig {
            places = pls,
            transitions = trns
            },
        maxCapacity = maxCap,
        newArrowsWithComplement = (newFlowMin, newFlowMax),
        oneMinCapacity = oneMin,
        distractors = (distractMin, distractMax),
        atMostActive = atMostAct
        } :: CapacityConfig
  let c = checkCapacityConfigs config
  if isNothing c
  then do
    t <- capacityGenerate config 0 i
    lift . (`withLang` English) $ simpleCapacityTask "tmp/" t
    lift $ print t
  else
    lift $ print c

validateInput :: Read a => a -> IO a
validateInput d = do
  input <- getLine
  if null input then return d
  else case readMaybe input of
    Just n  -> return n
    Nothing -> do
      putStrLn "Invalid input"
      validateInput d

userInput :: CapacityConfig -> IO (Int, Int, Int, Int, Int, Int, Int, Int, Maybe Int)
userInput CapacityConfig{
  basicConfig = BasicConfig{..},
  maxCapacity = maxCapacity,
  newArrowsWithComplement = (minNewFlowMin, minNewFlowMax),
  oneMinCapacity = oneMinCapacity,
  distractors = (distractorsMin, distractorsMax),
  atMostActive = atMost
  } = do
  putStr "Number of Places: "
  pls <- validateInput places
  putStr "Number of Transitions: "
  trns <- validateInput transitions
  putStr "Highest capacity for a place: "
  maxCap <- validateInput maxCapacity
  putStr "How many new flows are at minimum connected to complement places: "
  newFlowMin <- validateInput minNewFlowMin
  putStr "How many new flows are at maximum connected to complement places: "
  newFlowMax <- validateInput minNewFlowMax
  putStr "What capacity should one place at least have: "
  oneMin <- validateInput oneMinCapacity
  putStr "How many distractors (transitions that are activated, but not given the capacity) at minimum: "
  distractMin <- validateInput distractorsMin
  putStr "How many distractors (transitions that are activated, but not given the capacity) at maximum: "
  distractMax <- validateInput distractorsMax
  putStr "Maximum number of active Transitions (Just Int/Nothing): "
  atMostAct <- validateInput atMost
  return (pls, trns, maxCap, newFlowMin, newFlowMax, oneMin, distractMin, distractMax, atMostAct)
