{-# Language DuplicateRecordFields #-}

module Main (main) where 

import Common (
  forceErrors,
  instanceInput,
  )
import Modelling.PetriNet.Conflict (
  checkFindConflictConfig,
  checkPickConflictConfig,
  findConflictGenerate,
  findConflictTask,
  pickConflictGenerate,
  pickConflictTask,
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..), ChangeConfig(..), FindConflictConfig(..),
  PickConflictConfig(..),
  defaultFindConflictConfig, defaultPickConflictConfig,
  )

import Control.Monad.Output             (LangM' (withLang), Language (English))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Maybe                       (isNothing)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)

main :: IO()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "What type would you like? a: Find a Conflict in a Net, b: Choose the Net with the Conflict"
  sw <- getLine
  i <- instanceInput
  if i >= 0 
  then if sw == "b" then mainPick i else mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  pPrint defaultFindConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultFindConflictConfig {
        basicConfig = (bc defaultFindConflictConfig) {
            places = pls,
            transitions = trns
            },
        changeConfig = (cc defaultFindConflictConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: FindConflictConfig
  let c = checkFindConflictConfig config
  if isNothing c
  then do
    t <- findConflictGenerate config 0 i
    lift . (`withLang` English) $ findConflictTask "" t
    lift $ print t
  else
    lift $ print c
  where
    bc :: FindConflictConfig -> BasicConfig
    bc = basicConfig
    cc :: FindConflictConfig -> ChangeConfig
    cc = changeConfig

mainPick :: Int -> IO ()
mainPick i = forceErrors $ do
  pPrint defaultPickConflictConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultPickConflictConfig {
        basicConfig = (bc defaultPickConflictConfig) {
            places = pls,
            transitions = trns
            },
        changeConfig = (cc defaultPickConflictConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            }
        } :: PickConflictConfig
  let c = checkPickConflictConfig config
  if isNothing c
  then do
    t <- pickConflictGenerate config 0 i
    lift . (`withLang` English) $ pickConflictTask "" t
    lift $ print c
  else
    lift $ print c
  where
    bc :: PickConflictConfig -> BasicConfig
    bc = basicConfig
    cc :: PickConflictConfig -> ChangeConfig
    cc = changeConfig
    
userInput :: IO (Int,Int,Int,Int)
userInput = do
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  putStr "TokenChange Overall: "
  tknCh <- getLine
  putStr "FlowChange Overall: "
  flwCh <- getLine
  return (read pls, read trns,read tknCh, read flwCh)
