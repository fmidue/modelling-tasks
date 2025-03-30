{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Modelling.PetriNet.Capacity (
  capacityGenerate,
  capacityTask,
  findCapacity,
  petriNetFindCapacity,
  petriNetPickCapacity,
  parseCapacity,
  pickCapacity,
  ) where

import qualified Modelling.PetriNet.Types         as Find (
  AlloyConfig (maxInstances, timeout),
  CapacityConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  CapacityConfig (..),
  )
import qualified Data.Map                         as M (
  empty,
  fromList,
  )
import qualified Data.Set                         as Set (
  toList,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Object,
  oneOf,
  parseWith,
  )
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  defaultConstraints,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  randomInSegment,
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  )
import Modelling.PetriNet.Diagram (
  getDefaultNet,
  renderWith,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  findTaskInstance,
  )
import Modelling.PetriNet.Reach.Type (
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  ActivatedTransitions (ActivatedTransitions),
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  Capacity (Capacity),
  CapacityConfig (..),
  Drawable,
  DrawSettings (..),
  GraphConfig (..),
  Net (..),
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  CapacityNode (..),
  SimplePetriNet,
  petriScopeBitWidth,
  transitionListShow,
  transitionNames,
  placeNames,
  randomDrawSettings,
  manyRandomDrawSettings,
  )

import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM',
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  continueOrAbort,
  english,
  german,
  printSolutionAndAssert,
  translate,
  translations,
  unLangM
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.Trans              (MonadTrans (lift))
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance
  )

import GHC.Generics                     (Generic)
import Control.Monad.IO.Class (liftIO, MonadIO)


data CapacityInstance = CapacityInstance {
  drawWith :: !DrawSettings,
  toFind :: !(ActivatedTransitions Transition),
  originalNet :: !(PetriLike CapacityNode String),
  transformedNet :: !(PetriLike SimpleNode String),
  numberOfPlaces :: !Int,
  numberOfTransitions :: !Int,
  showSolution :: !Bool
  }
  deriving (Generic, Read, Show)

capacityGenerate
  :: (MonadAlloy m, MonadThrow m, MonadIO m)
  => CapacityConfig
  -> Int
  -> Int
  -> m CapacityInstance
capacityGenerate config seed segment =
  flip evalRandT (mkStdGen seed) $ do
    gl <- oneOf $ graphLayouts gc

    tn <- pickCapacity petriNetPickCapacity Pick.alloyConfig config segment

    (net, condition) <- findCapacity config segment
    condition' <- lift $ traverse (parseWith parseTransitionPrec) condition
    liftIO $ print net
    liftIO $ print condition
    liftIO $ print tn
    return $ CapacityInstance
      { drawWith = DrawSettings
          { withPlaceNames = not $ hidePlaceNames gc
          , withSvgHighlighting = True
          , withTransitionNames = not $ hideTransitionNames gc
          , with1Weights = not $ hideWeight1 gc
          , withGraphvizCommand = gl
          }
      , toFind = condition'
      , originalNet = tn
      , transformedNet = net
      , numberOfPlaces = places bc
      , numberOfTransitions = transitions bc
      , showSolution = Find.printSolution config
      }
      where
        bc = Find.basicConfig config
        gc = Pick.graphConfig config

capacityTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> CapacityInstance
  -> LangM m
capacityTask path task = do
  image
    $=<< renderWith path "capacity" (originalNet task) (drawWith task)
  paragraph $ translate $ do
    english [iii|
      State your answer by giving the number of the Petri net candidate
      that is syntactically incorrect.
      #{" "}|]
    german [iii|
      Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzkandidaten an,
      der syntaktisch inkorrekt ist.
      #{" "}|]
    pure ()
  paragraph hoveringInformation
  pure ()

findCapacity
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => CapacityConfig
  -> Int
  -> RandT
    g
    m
    (PetriLike SimpleNode String, ActivatedTransitions String)
findCapacity = taskInstance
  findTaskInstance
  petriNetFindCapacity
  parseCapacity
  Find.alloyConfig

pickCapacity
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => (config -> String)
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> RandT
    g
    m
    (PetriLike CapacityNode String)
pickCapacity alloyF alloyC config segment = do
  let is = Find.maxInstances (alloyC config)
  list <- getInstances is (Find.timeout $ alloyC config) (alloyF config)
  inst <- case fromIntegral <$> is of
    Nothing -> randomInstance list
    Just n -> do
      x <- randomInSegment segment n
      case drop x list of
        x':_ -> return x'
        []   -> randomInstance list
  getDefaultNet inst
  where
  randomInstance list = do
    n <- randomInSegment segment (1 + ((length list - segment - 1) `div` 4))
    return $ list !! n

petriNetFindCapacity :: CapacityConfig -> String
petriNetFindCapacity CapacityConfig {
  basicConfig,
  advConfig,
  maxCapacity
  }
  = petriNetFindCapacityAlloy
    basicConfig
    advConfig
    maxCapacity

petriNetPickCapacity :: CapacityConfig -> String
petriNetPickCapacity CapacityConfig{
  basicConfig,
  advConfig,
  maxCapacity
  } =
  petriNetFindCapacityAlloy
    basicConfig
    advConfig
    maxCapacity

parseCapacity :: MonadThrow m => AlloyInstance -> m (ActivatedTransitions Object)
parseCapacity inst = do
  t <- unscopedSingleSig inst activatedTransitions ""
  pure $ ActivatedTransitions (Set.toList t)

petriNetFindCapacityAlloy
  :: BasicConfig
  -> AdvConfig
  -> Int
  -> String
petriNetFindCapacityAlloy basicC advConfig maxCapacity
  = [i|module PetriNetCapacity

#{modulePetriSignature}
#{const modulePetriAdditions advConfig}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

sig placesWithCapacity extends givenPlaces
{
  capacity : one Int,
  complement : disj one addedPlaces
}
{
  capacity > 0
  capacity =< #{maxCapacity}
  complement.@tokens = minus[capacity, tokens]
}

fact {
  noChangesToGivenParts
  no addedTransitions
}

pred #{capacityPredicateName}[#{activated} : set Transitions] {
  #{defaultConstraints activated basicC}
  #{compAdvConstraints advConfig}

  all t : Transitions, p : givenPlaces |
    let n = minus[t.flow[p], p.flow[t]] |
      n < 0 implies (t.flow[p.complement] = minus[0, n] and no p.complement.flow[t])
      else
      n > 0 implies (p.complement.flow[t] = n and no t.flow[p.complement])
      else
      no p.complement.flow[t] and no t.flow[p.complement]
  all p : placesWithCapacity, w : p.flow[Transitions] + Transitions.flow[p] | p.capacity >= w

}

run #{capacityPredicateName} for exactly #{places basicC} givenPlaces, exactly #{places basicC} addedPlaces, exactly #{transitions basicC} Transitions, #{petriScopeBitWidth basicC} Int
|]
  where
    activated = skolemName

capacityPredicateName :: String
capacityPredicateName = "showCapacity"

activatedTransitions :: String
activatedTransitions = skolemVariable capacityPredicateName skolemName

skolemName :: String
skolemName = "activatedTrans"

