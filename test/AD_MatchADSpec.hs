module AD_MatchADSpec where

import AD_MatchAD (MatchADConfig(..), checkMatchADConfig, defaultMatchADConfig)

import AD_Config (ADConfig(minActions, minObjectNodes), defaultADConfig)
import Test.Hspec (Spec, describe, it, context, shouldBe, shouldSatisfy)
import Data.Maybe (isJust)

spec :: Spec
spec = describe "checkMatchADConfig" $ do
  it "checks if the basic Input is in given boundaries" $
    checkMatchADConfig defaultMatchADConfig `shouldBe` Nothing
  context "when provided with Input out of the constraints" $
    it "it returns a String with necessary changes" $
      checkMatchADConfig MatchADConfig {
        adConfig=defaultADConfig{minActions=0, minObjectNodes=0}
      } `shouldSatisfy` isJust