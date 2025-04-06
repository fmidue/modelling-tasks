module Modelling.PetriNet.AlloySpec where

import Modelling.PetriNet.Types (
  defaultBasicConfig,
  petriScopeBitWidth,
  )

import Test.Hspec

spec :: Spec
spec = do
  describe "petriScopeBitWidth" $
    context "computes the needed bit width for generating Petri nets with Alloy" $
      it "taking some values out of the user's input" $
        petriScopeBitWidth (basicConfigBitWidthInput basicC) `shouldSatisfy` (< 7)
