module Main (main) where 

import Modelling.PetriNet.FindConflicts
import Modelling.PetriNet.BasicNetFunctions
import Modelling.PetriNet.Types

import Data.Maybe                        (isNothing)
import System.IO

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  (pls,trns) <- userInput
  let config = defaultPetriBasicConfig{places = pls, transitions = trns}
  let c = checkBasicConfig config
  if isNothing c
  then do 
    _ <- findConflicts config
    print "finished Task2"
  else
    print c


userInput :: IO (Int,Int)
userInput = do   
  putStr "Number of Places: "
  pls <- getLine
  putStr "Number of Transitions: "
  trns <- getLine
  return (read pls, read trns)