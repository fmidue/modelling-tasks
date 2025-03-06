{-# Language DuplicateRecordFields #-}
{-# Language RecordWildCards #-}

module Main (main) where


import qualified Modelling.PetriNet.Types         as Find (
  FindActivatedTransitionsConfig (..),
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
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.Maybe                       (isNothing)
import System.IO (
  BufferMode (NoBuffering), hSetBuffering, stdout,
  )
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Generating instance for finding active transition(s) in a net"
  i <- instanceInput
  if i >= 0
  then mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  pPrint defaultFindActivatedTransitionsConfig
  (pls, trns, tknChange, flwChange, atMost) <- lift userInput
  let config = defaultFindActivatedTransitionsConfig {
        Find.basicConfig = (Find.basicConfig defaultFindActivatedTransitionsConfig) {
            places = pls,
            transitions = trns
            },
        Find.changeConfig = (Find.changeConfig defaultFindActivatedTransitionsConfig) {
            tokenChangeOverall = tknChange,
            flowChangeOverall = flwChange
            },
        Find.atMostActive = atMost
        } :: FindActivatedTransitionsConfig
  let c = checkFindActivatedTransitionsConfig config
  if isNothing c
  then do
    t <- findActivatedTransitionsGenerate config 0 i
    lift . (`withLang` English) $ simpleFindActivatedTransitionsTask "" t
    lift $ print t
  else
    lift $ print c

userInput :: IO (Int, Int, Int, Int, Int)
userInput = do
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  putStr "TokenChange Overall: "
  tknCh <- getLine
  putStr "FlowChange Overall: "
  flwCh <- getLine
  putStr "AtMostActive Transitions: "
  atMost <- getLine
  return (read pls, read trns, read tknCh, read flwCh, read atMost)

