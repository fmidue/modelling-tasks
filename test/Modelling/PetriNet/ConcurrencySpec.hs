module Modelling.PetriNet.ConcurrencySpec where

import Modelling.PetriNet.Concurrency
import Modelling.PetriNet.Types
  (defaultFindConcurrencyConfig,defaultPickConcurrencyConfig)

import Test.Hspec

spec :: Spec
spec = do
  describe "findConcurrency" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        (_,diaConc) <- findConcurrency defaultFindConcurrencyConfig 
        print (map snd diaConc) `shouldReturn` ()
  describe "pickConcurrency" $
    context "creates, out of a given Config," $
      it "everything needed to create the Task is generated" $ do
        (_,diaConc) <- pickConcurrency defaultPickConcurrencyConfig 
        print (map snd diaConc) `shouldReturn` ()