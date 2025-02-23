{-# Language DuplicateRecordFields #-}

module Main (main) where

import qualified Modelling.PetriNet.Types         as Pick (
  PickPossibleMistakeConfig (..),
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
  checkPickPossibleMistakeConfig,
  pickMistakeGenerate,
  simplePickMistakeTask,
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  PossibleMistakeConfig (..),
  PickPossibleMistakeConfig (..),
  defaultPickPossibleMistakeConfig,
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
  lift $ pPrint defaultPickPossibleMistakeConfig
  (pls, trns, tknChange, flwChange, negTokCost, transToTr, placeToPl) <- lift userInput
  let config = defaultPickPossibleMistakeConfig {
        Pick.basicConfig = (Pick.basicConfig defaultPickPossibleMistakeConfig) {
            places = pls,
            transitions = trns
            },
        Pick.changeConfig = (Pick.changeConfig defaultPickPossibleMistakeConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            },
        Pick.possibleMistakeConfig = (Pick.possibleMistakeConfig defaultPickPossibleMistakeConfig) {
            canHaveNegativeTokenCost = negTokCost,
            canHaveTransitionToTransition = transToTr,
            canHavePlaceToPlace = placeToPl
            }
        } :: PickPossibleMistakeConfig
  let c = checkPickPossibleMistakeConfig config
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
