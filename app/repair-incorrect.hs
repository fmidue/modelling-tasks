module Main where

import Capabilities.Alloy.IO.Trans            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO.Trans         ()
import Capabilities.Graphviz.IO.Trans         ()
import Capabilities.Exceptions.IO.Trans       ()
import Common                           (withLang)
import Modelling.CdOd.RepairCd (
  defaultRepairCdConfig,
  repairCd,
  repairCdTask,
  )
import Modelling.CdOd.SelectValidCd
  (defaultSelectValidCdConfig, selectValidCd, selectValidCdTask)
import EvaluateArgs                     (evaluateArgs)

import Control.OutputCapable.Blocks     (Language (English))
import System.Environment               (getArgs)

main :: IO ()
main = do
  repair:args <- getArgs
  (s, seed)   <- evaluateArgs args
  putStrLn $ "Seed: " ++ show seed
  putStrLn $ "Segment: " ++ show s
  if read repair
    then do
    task <- repairCd defaultRepairCdConfig s seed
    print task
    repairCdTask True "repair" task `withLang` English
    else do
    inst <- selectValidCd defaultSelectValidCdConfig s seed
    print inst
    selectValidCdTask True "select" inst `withLang` English
