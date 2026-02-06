module Main (main) where

import Common                           (withLang)

import Capabilities.Alloy.IO.Trans            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO.Trans         ()
import Capabilities.Exceptions.IO.Trans       ()
import Capabilities.Graphviz.IO.Trans         ()
import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNamesTask)
import Modelling.CdOd.Generate.DifferentNames (differentNames)
import EvaluateArgs                     (evaluateArgs)

import Control.OutputCapable.Blocks     (Language (English))
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  i <- differentNames 10 defaultDifferentNamesConfig s seed
  print i
  differentNamesTask True "output" i `withLang` English
