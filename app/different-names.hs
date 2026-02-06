module Main (main) where

import Capabilities.Alloy.IO.Trans      ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO.Trans   ()
import Capabilities.Graphviz.IO.Trans   ()
import Capabilities.Exceptions.IO.Trans ()
import Common                           (withLang)

import Modelling.CdOd.DifferentNames
  (defaultDifferentNamesConfig, differentNames, differentNamesTask)
import EvaluateArgs                     (evaluateArgs)

import Control.OutputCapable.Blocks     (Language (English))
import Control.Monad.Trans.Except       (runExceptT)
import System.Environment               (getArgs)

main :: IO ()
main = do
  (s, seed) <- getArgs >>= evaluateArgs
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  i <- either error id
     <$> runExceptT (differentNames defaultDifferentNamesConfig s seed)
  print i
  differentNamesTask True "output" i `withLang` English
