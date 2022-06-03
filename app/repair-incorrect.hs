module Main where

import Common                           ()
import Modelling.CdOd.RepairCd (
  defaultRepairCdConfig,
  repairCd,
  repairCdTask,
  )
import Modelling.CdOd.SelectValidCd
  (defaultSelectValidCdConfig, selectValidCd, selectValidCdTask)
import EvaluateArgs                     (evaluateArgs)

import Control.Monad.Output             (LangM' (withLang), Language (English))
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
    repairCdTask "repair" task `withLang` English
    else do
    inst <- selectValidCd defaultSelectValidCdConfig s seed
    print inst
    selectValidCdTask "select" inst `withLang` English
