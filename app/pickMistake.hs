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
import Modelling.PetriNet.PickMistake (
  checkPickMistakeConfig,
  pickMistakeGenerate,
  simplePickMistakeTask,
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  MistakeConfig (..),
  PickMistakeConfig (..),
  defaultPickMistakeConfig,
  )

import Control.OutputCapable.Blocks      (Language (English))
import Control.Monad.Trans.Class         (lift)
import Data.Char                         (toLower)
import Data.Maybe                        (isNothing)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)
import Text.Read                         (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Generating instance for picking the Net with mistakes"
  i <- instanceInput
  if i >= 0
  then mainPick i
  else print "There is no negative index"

mainPick :: Int -> IO ()
mainPick i = forceErrors $ do
  let theConfig@PickMistakeConfig{..} = defaultPickMistakeConfig
  lift $ pPrint theConfig
  (pls, trns, tknChange, flwChange, negTokCost, transToTr, placeToPl) <- lift $ userInput theConfig
  let config = theConfig {
        basicConfig = basicConfig {
            places = pls,
            transitions = trns
            },
        changeConfig = changeConfig {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            },
        mistakeConfig = mistakeConfig {
            canHaveNegativeWeight = negTokCost,
            canHaveTransitionToTransition = transToTr,
            canHavePlaceToPlace = placeToPl
            }
        } :: PickMistakeConfig
  let c = checkPickMistakeConfig config
  if isNothing c
  then do
    t <- pickMistakeGenerate config 0 i
    lift . (`withLang` English) $ simplePickMistakeTask "tmp/" t
    lift $ print t
  else
    lift $ print c

boolInput :: Bool -> IO Bool
boolInput d = do
  input <- getLine
  case map toLower input of
    "" -> return d
    "true"  -> return True
    "false" -> return False
    _       -> do
      putStrLn "Invalid input"
      boolInput d

intInput :: Int -> IO Int
intInput d = do
  input <- getLine
  if null input then return d
  else case readMaybe input of
    Just n  -> return n
    Nothing -> do
      putStrLn "Invalid input"
      intInput d

userInput :: PickMistakeConfig -> IO (Int, Int, Int, Int, Bool, Bool, Bool)
userInput PickMistakeConfig{basicConfig = BasicConfig{..}, changeConfig = ChangeConfig{..}, mistakeConfig = MistakeConfig{..}} = do
  putStr "Number of Places: "
  pls <- intInput places
  putStr "Number of Transitions: "
  trns <- intInput transitions
  putStr "TokenChange Overall: "
  tknCh <- intInput tokenChangeOverall
  putStr "FlowChange Overall: "
  flwCh <- intInput flowChangeOverall
  putStr "Negative Token Cost (True/False): "
  negTokCost <- boolInput canHaveNegativeWeight
  putStr "Transition to Transition (True/False): "
  transToTr <- boolInput canHaveTransitionToTransition
  putStr "Places to Places (True/False): "
  placeToPl <- boolInput canHavePlaceToPlace
  return (pls, trns, tknCh, flwCh, negTokCost, transToTr, placeToPl)
