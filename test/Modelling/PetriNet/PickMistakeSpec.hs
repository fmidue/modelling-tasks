{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.PickMistakeSpec where

import qualified Modelling.PetriNet.Types         as Pick (
  PickMistakeConfig (..),
  )

import Modelling.PetriNet.PickMistake (
  checkMistakeConfig,
  checkPickMistakeConfig,
  petriNetPickMist,
  pickMistake,
  )

import Modelling.PetriNet.Pick (
  pickTaskInstance,
  )
import Modelling.PetriNet.Types (
  BasicConfig,
  ChangeConfig,
  MistakeConfig (..),
  PickMistakeConfig (..),
  SimplePetriLike,
  defaultPickMistakeConfig,
  )

import Modelling.PetriNet.TestCommon (
  alloyTestConfig,
  checkConfigs,
  defaultConfigTaskGeneration,
  firstInstanceConfig,
  testTaskGeneration,
  validConfigsForPick,
  validGraphConfig,
  )
import Settings                         (configDepth)

import Control.Lens.Lens                ((??))
import Data.Functor.Const               (Const(..))
import Data.Maybe                       (isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultPickMistakeConfig" $
    checkConfigs checkPickMistakeConfig [defaultPickMistakeConfig]
  describe "validPickMistakeConfigs" $
    checkConfigs checkPickMistakeConfig pickConfigs
  describe "pickMistake" $ do
    defaultConfigTaskGeneration
      (pickMistake defaultPickMistakeConfig {
          Pick.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkPickMistakeInstance @(SimplePetriLike _)
    testPickMistakeConfig pickConfigs
  where
    pickConfigs = validPickMistakeConfigs validPicks
    validPicks = validConfigsForPick 0 configDepth

checkPickMistakeInstance :: [(a, Maybe (Const () String))] -> Bool
checkPickMistakeInstance = f . fmap snd
  where
    f [Just (Const ()), Nothing] = True
    f _                          = False

testPickMistakeConfig :: [PickMistakeConfig] -> Spec
testPickMistakeConfig = testTaskGeneration
  petriNetPickMist
  (pickTaskInstance (const (return (Const ()))))
  $ checkPickMistakeInstance @(SimplePetriLike _)

validMistakeConfigs :: BasicConfig -> ChangeConfig -> [MistakeConfig]
validMistakeConfigs bc ch = filter (isNothing . checkMistakeConfig bc ch) $ do
  negative <- [False, True]
  transitionToTransition <- [False, True]
  placeToPlace <- [False, True]
  [MistakeConfig negative transitionToTransition placeToPlace]

validPickMistakeConfigs
  :: [(BasicConfig, ChangeConfig)]
  -> [PickMistakeConfig]
validPickMistakeConfigs cs = do
  (bc, ch) <- cs
  PickMistakeConfig bc ch
    <$> validMistakeConfigs bc ch
    <*> pure validGraphConfig
    <*> pure False
    ?? False
    ?? alloyTestConfig
