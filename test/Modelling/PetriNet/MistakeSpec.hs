{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.PetriNet.MistakeSpec where

import qualified Modelling.PetriNet.Types         as Pick (
  PickMistakeConfig (..),
  )

import Modelling.PetriNet.Mistake (
  checkMistakeConfig,
  checkPickMistakeConfig,
  petriNetPickMist,
  parseMistake,
  pickMistake,
  )

import Modelling.PetriNet.Pick (
  pickTaskInstance,
  )
import Modelling.PetriNet.Types (
  BasicConfig,
  ChangeConfig,
  MistakeConfig (..),
  Mistakes (Mistakes),
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
import Settings                         (configDepth, needsTuning)

import Control.Lens.Lens                ((??))
import Data.Maybe                       (isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "validPickMistakeConfigs" $
    checkConfigs checkPickMistakeConfig pickConfigs
  describe "pickMistake" $ do
    defaultConfigTaskGeneration
      (pickMistake defaultPickMistakeConfig {
          Pick.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkPickMistakeInstance @(SimplePetriLike _)
    needsTuning $
      testPickMistakeConfig pickConfigs
  where
    pickConfigs = validPickMistakeConfigs validPicks
    validPicks = validConfigsForPick 0 configDepth

checkPickMistakeInstance :: [(a, Maybe (Mistakes String))] -> Bool
checkPickMistakeInstance = f . fmap snd
  where
    f [Just x, Nothing] = isValidMistake x
    f _                 = False

testPickMistakeConfig :: [PickMistakeConfig] -> Spec
testPickMistakeConfig = testTaskGeneration
  petriNetPickMist
  (pickTaskInstance parseMistake)
  $ checkPickMistakeInstance @(SimplePetriLike _)

validMistakeConfigs :: BasicConfig -> ChangeConfig -> [MistakeConfig]
validMistakeConfigs bc ch = filter (isNothing.checkMistakeConfig bc ch) $ do
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

isValidMistake :: Mistakes String -> Bool
isValidMistake m@(Mistakes (t1, t2, p1, p2))
  | ('t':x) <- t1, ('t':y) <- t2, x /= y = True
  | ('p':x) <- p1, ('p':y) <- p2, x /= y = True
  | otherwise                            = error $ show m
