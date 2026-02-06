module Main (main) where

import Capabilities.Alloy.IO.Trans            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO.Trans         ()
import Capabilities.Graphviz.IO.Trans         ()
import Capabilities.PlantUml.IO         ()
import Capabilities.WriteFile.IO        ()
import Capabilities.Exceptions.IO.Trans       ()
import Modelling.ActivityDiagram.MatchPetri (
  defaultMatchPetriConfig,
  matchPetri,
  matchPetriTask,
  matchPetriSyntax,
  matchPetriEvaluation
  )
import Control.OutputCapable.Blocks     (Language (English))
import System.Environment               (getArgs)

import Common                           (withLang)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [path, s, seed] -> do
      putStrLn $ "Segment: " ++ s
      putStrLn $ "Seed: " ++ seed
      task <- matchPetri defaultMatchPetriConfig (read s) (read seed)
      print task
      matchPetriTask path task `withLang` English
      sub <- read <$> getLine
      matchPetriSyntax task sub `withLang` English
      points <- matchPetriEvaluation task sub `withLang` English
      putStrLn $ "Points: " ++ show points
    _ -> error "usage: three parameters required: FilePath (Output Folder) Segment (Int) Seed (Int)"
