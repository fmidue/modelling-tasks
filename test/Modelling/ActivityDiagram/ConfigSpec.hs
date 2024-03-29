module Modelling.ActivityDiagram.ConfigSpec where

import Modelling.ActivityDiagram.Config (
  ADConfig(..),
  checkADConfig,
  defaultADConfig,
  adConfigBitWidth,
  adConfigToAlloy',
  adConfigScope,
  )

import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)
import Language.Alloy.Call (getInstances)


spec :: Spec
spec = do
  describe "checkADConfig" $ do
    it "checks if the basic Input is in given boundaries" $
      checkADConfig defaultADConfig `shouldBe` Nothing
    context "when provided with Input out of the constraints" $
      it "it returns a String with necessary changes" $
        checkADConfig defaultADConfig{minActions=0, minObjectNodes=0}
          `shouldSatisfy` isJust
  describe "adConfigScope" $
    context "given the default config" $ do
      let conf = defaultADConfig
          bitWidth = adConfigBitWidth conf
      it "provides a scope large enough to generate instances" $ do
        let spec' = adConfigToAlloy' (adConfigScope conf) bitWidth "" "" conf
            depth = 10
        inst <- getInstances (Just depth) spec'
        length inst `shouldBe` fromIntegral depth
      it "provides a scope where half of it would not be sufficient to generate instances" $ do
        let spec' = adConfigToAlloy' (adConfigScope conf `div` 2) bitWidth "" "" conf
        inst <- getInstances (Just 1) spec'
        length inst `shouldBe` (0 :: Int)
  describe "adConfigBitWidth" $
    context "given the default config" $ do
      let conf = defaultADConfig
          scope = adConfigScope conf
      it "provides a bit width large enough to generate instances" $ do
        let spec' = adConfigToAlloy' scope (adConfigBitWidth conf) "" "" conf
            depth = 10
        inst <- getInstances (Just depth) spec'
        length inst `shouldBe` fromIntegral depth
      it "provides a bit width where half of it would not be sufficient to generate instances" $ do
        let spec' = adConfigToAlloy' scope (adConfigBitWidth conf `div` 2) "" "" conf
        inst <- getInstances (Just 1) spec'
        length inst `shouldBe` (0 :: Int)
