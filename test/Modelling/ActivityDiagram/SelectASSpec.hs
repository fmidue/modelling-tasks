module Modelling.ActivityDiagram.SelectASSpec where

import Modelling.ActivityDiagram.SelectAS (SelectASConfig(..), checkSelectASConfig, defaultSelectASConfig)

import Modelling.ActivityDiagram.Config (
  AdConfig (objectNodeLimits, cycles),
  defaultAdConfig,
  )
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkSelectASConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkSelectASConfig defaultSelectASConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $ do
    it "it returns a String with necessary changes" $
      checkSelectASConfig defaultSelectASConfig {
        adConfig = defaultAdConfig {objectNodeLimits = (0, 1)},
        objectNodeOnEveryPath = Just True
      } `shouldSatisfy` isJust
    it "requireActionRepetition without cycles should fail" $
      checkSelectASConfig defaultSelectASConfig {
        adConfig = defaultAdConfig {cycles = 0},
        requireActionRepetition = Just True
      } `shouldSatisfy` isJust
  context "when requireActionRepetition is configured correctly" $
    it "should pass validation with cycles" $
      checkSelectASConfig defaultSelectASConfig {
        adConfig = defaultAdConfig {cycles = 1},
        requireActionRepetition = Just True
      } `shouldBe` Nothing
