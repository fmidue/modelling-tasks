{-# Language DuplicateRecordFields #-}

module Main (main) where

import qualified Modelling.PetriNet.Types         as Pick (
  PickMistakeConfig (..),
  )

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Common (
  forceErrors,
  instanceInput,
  withLang,
  )
import Modelling.PetriNet.Mistake (
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
  lift $ pPrint defaultPickMistakeConfig
  (pls, trns, tknChange, flwChange, negTokCost, transToTr, placeToPl) <- lift userInput
  let config = defaultPickMistakeConfig {
        Pick.basicConfig = (Pick.basicConfig defaultPickMistakeConfig) {
            places = pls,
            transitions = trns
            },
        Pick.changeConfig = (Pick.changeConfig defaultPickMistakeConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            },
        Pick.mistakeConfig = (Pick.mistakeConfig defaultPickMistakeConfig) {
            negativeTokenCost = negTokCost,
            transitionToTransition = transToTr,
            placeToPlace = placeToPl
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

boolInput :: IO Bool
boolInput = do
  input <- getLine
  case map toLower input of
    "true"  -> return True
    "false" -> return False
    _       -> do
      putStrLn "Invalid input"
      boolInput

intInput :: IO Int
intInput = do
  input <- getLine
  case readMaybe input of
    Just n  -> return n
    Nothing -> do
      putStrLn "Invalid input"
      intInput

userInput :: IO (Int, Int, Int, Int, Bool, Bool, Bool)
userInput = do
  putStr "Number of Places: "
  pls <- intInput
  putStr "Number of Transitions: "
  trns <- intInput
  putStr "TokenChange Overall: "
  tknCh <- intInput
  putStr "FlowChange Overall: "
  flwCh <- intInput
  putStr "Negative Token Cost (True/False): "
  negTokCost <- boolInput
  putStr "Transition to Transition (True/False): "
  transToTr <- boolInput
  putStr "Places to Places (True/False): "
  placeToPl <- boolInput
  return (pls, trns, tknCh, flwCh, negTokCost, transToTr, placeToPl)
