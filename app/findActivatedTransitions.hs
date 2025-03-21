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
import Modelling.PetriNet.FindActivatedTransitions (
  checkFindActivatedTransitionsConfig,
  findActivatedTransitionsGenerate,
  simpleFindActivatedTransitionsTask,
  )
import Modelling.PetriNet.Types         (
  BasicConfig(..),
  ChangeConfig(..),
  FindActivatedTransitionsConfig(..),
  defaultFindActivatedTransitionsConfig,
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
  putStrLn "Generating instance for finding activated transition(s) in a net"
  i <- instanceInput
  if i >= 0
  then mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  let theConfig@FindActivatedTransitionsConfig{..} = defaultFindActivatedTransitionsConfig
  lift $ pPrint theConfig
  (pls, trns, tknChange, flwChange, atMost) <- lift $ userInput theConfig
  let config = theConfig {
        basicConfig = basicConfig {
            places = pls,
            transitions = trns
            },
        changeConfig = changeConfig {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            },
        atMostActive = atMost
        } :: FindActivatedTransitionsConfig
  let c = checkFindActivatedTransitionsConfig config
  if isNothing c
  then do
    t <- findActivatedTransitionsGenerate config 0 i
    lift . (`withLang` English) $ simpleFindActivatedTransitionsTask "tmp/" t
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

userInput :: FindActivatedTransitionsConfig -> IO (Int, Int, Int, Int, Maybe Int)
userInput FindActivatedTransitionsConfig{basicConfig = BasicConfig{..}, changeConfig = ChangeConfig{..}, atMostActive = atMostActiveValue} = do
  putStr "Number of Places: "
  pls <- validateInput places
  putStr "Number of Transitions: "
  trns <- validateInput transitions
  putStr "TokenChange Overall: "
  tknCh <- validateInput tokenChangeOverall
  putStr "FlowChange Overall: "
  flwCh <- validateInput flowChangeOverall
  putStr "AtMostActive Transitions (Just Int/Nothing): "
  atMost <- validateInput atMostActiveValue
  return (pls, trns, tknCh, flwCh, atMost)
