module Modelling.ActivityDiagram.PetrinetSpec where

import qualified Data.Map as M (keys)

import Modelling.ActivityDiagram.Petrinet (
  PetriKey (..),
  convertToPetrinet,
  convertToSimple,
  )

import Modelling.ActivityDiagram.Config (adConfigToAlloy, defaultADConfig)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith)
import Modelling.PetriNet.Types (PetriLike(allNodes), petriLikeToPetri)

import Language.Alloy.Call (getInstances)

import Data.Either (isRight)
import Data.List (sort)
import Test.Hspec(Spec, context, describe, it, shouldBe)

spec :: Spec
spec =
  describe "convertToPetrinet" $
    context "on a list of generated diagrams" $ do
      let spec' = adConfigToAlloy "" "" defaultADConfig
      it "generates a petrinet with ascending labels" $ do
        inst <- getInstances (Just 50) spec'
        let petri = map (convertToSimple . failWith id . parseInstance) inst
        all checkLabels petri `shouldBe` (True::Bool)
      it "generates only valid petrinets" $ do
        inst <- getInstances (Just 50) spec'
        let petri = map (petriLikeToPetri . convertToPetrinet . failWith id . parseInstance) inst
        all isRight petri `shouldBe` (True::Bool)

checkLabels :: PetriLike n PetriKey -> Bool
checkLabels petri =
  let labels = sort $ map label $ M.keys $ allNodes petri
  in labels == [1..(length labels)]
