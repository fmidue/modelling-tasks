{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Modelling.PetriNet.FindActivatedTransitionsSpec where

import qualified Modelling.PetriNet.Types         as Find (
  FindActivatedTransitionsConfig (alloyConfig),
  )

import Modelling.PetriNet.FindActivatedTransitions (
  checkActivatedTransitionsConfig,
  checkFindActivatedTransitionsConfig,
  findActivatedTransitions,
  parseActivatedTransitions,
  petriNetFindActivatedTransitions,
  )

import Modelling.PetriNet.Find (
  findTaskInstance,
  )
import Modelling.PetriNet.Types (
  ActivatedTransitions (..),
  AdvConfig (AdvConfig),
  BasicConfig (..),
  ChangeConfig,
  FindActivatedTransitionsConfig (FindActivatedTransitionsConfig),
  SimplePetriLike,
  defaultFindActivatedTransitionsConfig,
  )

import Modelling.PetriNet.TestCommon (
  alloyTestConfig,
  checkConfigs,
  defaultConfigTaskGeneration,
  firstInstanceConfig,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForFind,
  validGraphConfig,
  )
import Settings                         (configDepth)

import Data.Maybe                       (isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultFindActivatedTransitionsConfig" $
    checkConfigs checkFindActivatedTransitionsConfig [defaultFindActivatedTransitionsConfig]
  describe "validFindActivatedTransitionsConfigs" $
    checkConfigs checkFindActivatedTransitionsConfig findConfigs'
  describe "findActivatedTransitions" $ do
    defaultConfigTaskGeneration
      (findActivatedTransitions defaultFindActivatedTransitionsConfig {
          Find.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkFindActivatedTransitionsInstance @(SimplePetriLike _)
    testFindActivatedTransitionsConfig findConfigs
  where
    findConfigs' = validFindActivatedTransitionsConfigs
      validFinds
      (AdvConfig Nothing Nothing Nothing)
    findConfigs = validAdvConfigs >>= validFindActivatedTransitionsConfigs validFinds
    validFinds = validConfigsForFind 0 configDepth

checkFindActivatedTransitionsInstance :: (a, ActivatedTransitions String) -> Bool
checkFindActivatedTransitionsInstance = isValidActivatedTransitions . snd

testFindActivatedTransitionsConfig :: [FindActivatedTransitionsConfig] -> Spec
testFindActivatedTransitionsConfig = testTaskGeneration
  petriNetFindActivatedTransitions
  (findTaskInstance parseActivatedTransitions)
  $ checkFindActivatedTransitionsInstance @(SimplePetriLike _)

validFindActivatedTransitionsConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> AdvConfig
  -> [FindActivatedTransitionsConfig]
validFindActivatedTransitionsConfigs cs advancedConfig = do
  (bc, ch) <- cs
  FindActivatedTransitionsConfig bc advancedConfig ch
    <$> validActivatedTransitionsConfigs bc
    <*> pure validGraphConfig
    <*> pure False
    <*> pure alloyTestConfig

validActivatedTransitionsConfigs :: BasicConfig -> [Maybe Int]
validActivatedTransitionsConfigs bc@BasicConfig{ transitions } = filter (isNothing . checkActivatedTransitionsConfig bc) $
  Nothing : [Just n | n <- [0 .. transitions - 1]]

isValidActivatedTransitions :: ActivatedTransitions String -> Bool
isValidActivatedTransitions _ = True
