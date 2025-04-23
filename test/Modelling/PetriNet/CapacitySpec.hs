{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Modelling.PetriNet.CapacitySpec where

import qualified Modelling.PetriNet.Types         as Find (
  CapacityConfig (alloyConfig),
  )

import Modelling.PetriNet.Capacity (
  checkCapacityConfigs,
  checkCapacityConfig,
  combinedCapacityInstance,
  findCapacityInstance,
  petriNetFindCapacity,
  )
import Modelling.PetriNet.Types (
  AdvConfig (AdvConfig),
  BasicConfig (..),
  CapacityConfig (CapacityConfig),
  CapacityNode (..),
  PetriLike (..),
  PetriChangeList (..),
  SimplePetriLike,
  defaultCapacityConfig,
  )

import Modelling.PetriNet.TestCommon (
  alloyTestConfig,
  checkConfigs,
  defaultConfigTaskGeneration,
  firstInstanceConfig,
  testTaskGeneration,
  validAdvConfigs,
  validConfigsForPick,
  validGraphConfig,
  )
import Settings                         (configDepth)
import Data.Maybe                       (fromMaybe, isNothing)

import Test.Hspec

spec :: Spec
spec = do
  describe "defaultCapacityConfig" $
    checkConfigs checkCapacityConfigs [defaultCapacityConfig]
  describe "validFindCapacityConfigs" $
    checkConfigs checkCapacityConfigs findConfigs'
  describe "combinedCapacity" $ do
    defaultConfigTaskGeneration
      (combinedCapacityInstance defaultCapacityConfig {
          Find.alloyConfig = firstInstanceConfig
          } 0)
      0
      $ checkCapacityInstance @(SimplePetriLike _)
    testCapacityConfig findConfigs
  where
    findConfigs' = validFindCapacityConfigs
      validFinds
      (AdvConfig Nothing Nothing Nothing)
    findConfigs = validAdvConfigs >>= validFindCapacityConfigs validFinds
    validFinds = validConfigsForFind 0 configDepth

checkCapacityInstance :: (PetriLike CapacityNode String, a, PetriChangeList String, [(String, String)]) -> Bool
checkCapacityInstance (_, _, change, _) = isValidCapacity change

testCapacityConfig :: [CapacityConfig] -> Spec
testCapacityConfig = testTaskGeneration
  petriNetFindCapacity
  findCapacityInstance
  $ checkCapacityInstance @(SimplePetriLike _)

validFindCapacityConfigs :: [(BasicConfig, _)] -> AdvConfig -> [CapacityConfig]
validFindCapacityConfigs cs advancedConfig = do
  (bc, _) <- cs
  (maxCapacity, newArrows, oneMin, distractors, atMost) <- validCapacityConfig bc
  return $ CapacityConfig bc advancedConfig maxCapacity newArrows oneMin distractors atMost validGraphConfig False alloyTestConfig

validCapacityConfig :: BasicConfig -> [(Int, (Int, Int), Int, (Int, Int), Maybe Int)]
validCapacityConfig bc@BasicConfig{ places, transitions, maxFlowPerEdge, maxTokensPerPlace, atLeastActive } =
  filter (\(maxCap, arrows, oneMinCap, distract, most) -> isNothing (checkCapacityConfig bc maxCap arrows oneMinCap distract most)) $ do
    maxCapacity <- [max maxFlowPerEdge maxTokensPerPlace .. 5]
    newArrows <- [(a, b) | a <- [places .. 2 * transitions * places], b <- [a .. 2 * transitions * places]]
    oneMin <- [1 .. maxCapacity]
    atMost <- Nothing : [Just n | n <- [atLeastActive .. transitions - 1]]
    distractors <- [(x, y) | x <- [0 .. transitions - fromMaybe transitions atMost], y <- [x .. transitions - atLeastActive]]
    return (maxCapacity, newArrows, oneMin, distractors, atMost)

isValidCapacity :: PetriChangeList String -> Bool
isValidCapacity c@(ChangeList {tokenChanges = ts, flowChanges = fs})
  | any (\(_, token) -> token < 0) ts                 = error $ show c
  | any (\(source, target, _) -> source == target) fs = error $ show c
  | any (\(_, _, flow) -> flow <= 0) fs               = error $ show c
  | otherwise                                         = True
