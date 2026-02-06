module Main (main) where

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Common                           (withLang)

import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNames, differentNamesTask)
import EvaluateArgs                     (evaluateArgs)

import Control.OutputCapable.Blocks     (Language (English))
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  i <- differentNames defaultDifferentNamesConfig s seed
  print i
  differentNamesTask True "output" i `withLang` English
