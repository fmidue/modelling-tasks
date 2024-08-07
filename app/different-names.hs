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
  differentNamesTask "output" i `withLang` English
