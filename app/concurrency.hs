{-# Language DuplicateRecordFields #-}

module Main (main) where

import Common                           (forceErrors, renderPetriNet)

import Modelling.PetriNet.Concurrency
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types (
  BasicConfig (..), ChangeConfig (..), FindConcurrencyConfig (..),
  PickConcurrencyConfig (..),
  defaultFindConcurrencyConfig, defaultPickConcurrencyConfig,
  )

import Control.Monad.Trans.Class        (lift)
import Data.Maybe                        (isNothing)
import Diagrams.Backend.SVG             (B)
import Diagrams.Prelude                 (Diagram)
import Maybes                            (firstJusts)
import System.IO
import Text.Pretty.Simple                (pPrint)

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  putStr "What type would you like? a: Find a concurrency in a Net, b: Choose the Net with the concurrency"
  sw <- getLine
  i <- instanceInput
  if i >= 0 
  then if sw == "b" then mainPick i else mainFind i
  else print "There is no negative index"

mainFind :: Int -> IO ()
mainFind i = forceErrors $ do
  lift $ pPrint defaultFindConcurrencyConfig
  (pls,trns,tknChange,flwChange) <- lift userInput
  let config = defaultFindConcurrencyConfig{
                           basicTask = (basicTask (defaultFindConcurrencyConfig :: FindConcurrencyConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultFindConcurrencyConfig :: FindConcurrencyConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: FindConcurrencyConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: FindConcurrencyConfig))
        , checkChangeConfig (basicTask (config :: FindConcurrencyConfig)) (changeTask (config :: FindConcurrencyConfig))
        , checkCConfig (basicTask (config ::FindConcurrencyConfig)) 
        ]
  if isNothing c
  then do
    conc <- findConcurrency i config
    lift $ putStrLn findConcurrencyTask
    lift $ printNetAndConcurrency "" conc
  else
    lift $ print (c :: Maybe String)
    
mainPick :: Int -> IO ()
mainPick i = forceErrors $ do
  lift $ pPrint defaultPickConcurrencyConfig
  (pls, trns, tknChange, flwChange) <- lift userInput
  let config = defaultPickConcurrencyConfig{
                           basicTask = (basicTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){places = pls, transitions = trns}
                         , changeTask = (changeTask (defaultPickConcurrencyConfig :: PickConcurrencyConfig)){ tokenChangeOverall = tknChange
                                                           , flowChangeOverall = flwChange}
                         } :: PickConcurrencyConfig
  let c = firstJusts 
        [ checkBasicConfig (basicTask (config :: PickConcurrencyConfig))
        , checkChangeConfig (basicTask (config :: PickConcurrencyConfig)) (changeTask (config :: PickConcurrencyConfig)) 
        ]
  if isNothing c
  then do
    concs <- pickConcurrency i config
    lift $ putStrLn pickConcurrencyTask
    lift $ uncurry printNetAndConcurrency `mapM_` zip (show <$> [1 :: Integer ..]) concs
  else
    lift $ print (c :: Maybe String)

printNetAndConcurrency :: Show a => String -> (Diagram B, a) -> IO ()
printNetAndConcurrency x (dia, concurrent) = do
  renderPetriNet x dia
  print concurrent

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
