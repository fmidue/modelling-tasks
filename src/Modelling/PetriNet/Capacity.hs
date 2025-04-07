{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Modelling.PetriNet.Capacity (
  capacityEvaluation,
  capacityGenerate,
  capacitySyntax,
  capacityTask,
  checkCapacityConfigs,
  defaultCapacityInstance,
  findCapacity,
  petriNetFindCapacity,
  petriNetPickCapacity,
  parseCapacity,
  pickCapacity,
  simpleCapacityTask,
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
  enforceConstraints,
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
  checkBasicConfig,
  findTaskInstance,
  prohibitHidePlaceNames,
  prohibitHideTransitionNames,
  prohibitPatchworkRenderer,
  toFindEvaluationList,
  )
import Modelling.PetriNet.Reach.Type (
  ShowTransition (ShowTransition),
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  ActivatedTransitions (ActivatedTransitions),
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  CapacityConfig (..),
  DrawSettings (..),
  GraphConfig (..),
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  CapacityNode (..),
  basicConfigBitWidthInput,
  checkActivatedSourceConfig,
  petriScopeBitWidth,
  )

import Control.Applicative              ((<|>))
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
import Data.Foldable                    (for_)
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance
  )

import GHC.Generics                     (Generic)


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
  :: (MonadAlloy m, MonadThrow m)
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

simpleCapacityTask
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
simpleCapacityTask = capacityTask

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
  paragraph $ translate $ do
    english "Consider the following Petri net with capacities:"
    german "Betrachten Sie folgendes Petrinetz mit Kapazitäten:"
  image
    $=<< renderWith path "capacity" (originalNet task) (drawWith task)
  image
    $=<< renderWith path "capacity" (transformedNet task) (drawWith task)
  paragraph $ do
    translate $ do
      english [iii|
        Given the isolated Places . With how many tokens and how should they be connected to Transitions so that the
        resulting Petri net without capacities is equivalent to the given Petri net with capacities?
        |]
      german [iii|
        Gegeben der isolierten Stellen. Mit wie vielen Marken und wie sollten diese mit Transitionen verbunden werden, sodass
        das resultierende Petrinetz ohne Kapazitäten äquivalent zum gegebenen Petrinetz mit Kapazitäten ist?
        |]
    translate $ do
      english [iii|
        State your answer by giving a tuple consisting of the complement places and their flows.
        #{" "}|]
      german [iii|
        Geben Sie Ihre Antwort in Form eines Tupels an, das aus den Komplementstellen und ihren Flüssen besteht.
        #{" "}|]
    pure ()
  paragraph hoveringInformation
  pure ()

capacitySyntax
  :: OutputCapable m
  => CapacityInstance
  -> [Transition]
  -> LangM' m ()
capacitySyntax task input = do
  for_ input assertTransition
  pure ()
  where
    assert = continueOrAbort False
    assertTransition t = assert (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a transition of the given Petri net?"
      german $ t' ++ " ist eine Transition des gegebenen Petrinetzes?"
    isValidTransition (Transition x) = x >= 1 && x <= numberOfTransitions task

capacityEvaluation
  :: (Monad m, OutputCapable m)
  => CapacityInstance
  -> [Transition]
  -> Rated m
capacityEvaluation task x = do
  let what = translations $ do
        english "are activated"
        german "sind aktiviert"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ toFindEvaluationList what withSol active x
  where
    active = capacitySolution task
    withSol = showSolution task

capacitySolution :: CapacityInstance -> [Transition]
capacitySolution task = active
  where
    ActivatedTransitions active = toFind task

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
  maxCapacity,
  minNewArrowsWithComplement,
  oneMinCapacity
  }
  = petriNetFindCapacityAlloy
    basicConfig
    advConfig
    maxCapacity
    minNewArrowsWithComplement
    oneMinCapacity

petriNetPickCapacity :: CapacityConfig -> String
petriNetPickCapacity CapacityConfig{
  basicConfig,
  advConfig,
  maxCapacity,
  minNewArrowsWithComplement,
  oneMinCapacity
  } =
  petriNetFindCapacityAlloy
    basicConfig
    advConfig
    maxCapacity
    minNewArrowsWithComplement
    oneMinCapacity

parseCapacity :: MonadThrow m => AlloyInstance -> m (ActivatedTransitions Object)
parseCapacity inst = do
  t <- unscopedSingleSig inst activatedTransitions ""
  pure $ ActivatedTransitions (Set.toList t)

petriNetFindCapacityAlloy
  :: BasicConfig
  -> AdvConfig
  -> Int
  -> Maybe Int
  -> Maybe Int
  -> String
petriNetFindCapacityAlloy basicC advConfig maxCapacity minNewArrowsWithComplement oneMinCapacity
  = [i|module PetriNetCapacity

#{modulePetriSignature}
#{modulePetriAdditions}
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

pred sinkTransitionsDefault[ts : set Transitions]{
  no ts.defaultFlow
}

pred sourceTransitionsDefault[ts : set Transitions]{
  no Nodes.defaultFlow[ts]
}

pred #{capacityPredicateName}[#{activated} : set Transitions] {
  #{defaultConstraintsAtLeastZero activated basicC}
  #{compAdvConstraints advConfig True}

  all t : Transitions, p : givenPlaces |
    let n = minus[t.flow[p], p.flow[t]] |
      n < 0 implies (t.flow[p.complement] = minus[0, n] and no p.complement.flow[t])
      else
      n > 0 implies (p.complement.flow[t] = n and no t.flow[p.complement])
      else
      no p.complement.flow[t] and no t.flow[p.complement]

  all p : placesWithCapacity, w : p.flow[Transitions] + Transitions.flow[p] | p.capacity >= w

  #{minNewArrowsWithComplementConstraints minNewArrowsWithComplement}
  #{oneMinCapacityConstraints oneMinCapacity}

}

run #{capacityPredicateName} for exactly #{places basicC} givenPlaces, exactly #{places basicC} addedPlaces,
exactly #{transitions basicC} Transitions, #{petriScopeBitWidth (basicConfigBitWidthInput basicC ++ [maxCapacity])} Int
|]
  where
    activated = skolemName
    minNewArrowsWithComplementConstraints :: Maybe Int -> String
    minNewArrowsWithComplementConstraints minNewArrows =
      case minNewArrows of
        Just minNew ->
          "#flowChange >= " ++ show minNew
        Nothing -> ""
    oneMinCapacityConstraints :: Maybe Int -> String
    oneMinCapacityConstraints oneMinCap =
      case oneMinCap of
        Just oneMin -> [i|some p : placesWithCapacity | p.capacity >= #{oneMin}|]
        Nothing -> ""

capacityPredicateName :: String
capacityPredicateName = "showCapacity"

activatedTransitions :: String
activatedTransitions = skolemVariable capacityPredicateName skolemName

skolemName :: String
skolemName = "activatedTrans"

checkCapacityConfigs :: CapacityConfig -> Maybe String
checkCapacityConfigs CapacityConfig {
  basicConfig,
  advConfig,
  maxCapacity,
  minNewArrowsWithComplement,
  oneMinCapacity,
  graphConfig
  }
  = prohibitHidePlaceNames graphConfig
  <|> prohibitHideTransitionNames graphConfig
  <|> checkBasicConfig basicConfig
  <|> prohibitPatchworkRenderer graphConfig
  <|> checkActivatedSourceConfig basicConfig advConfig
  <|> checkCapacityConfig basicConfig maxCapacity minNewArrowsWithComplement oneMinCapacity

checkCapacityConfig :: BasicConfig -> Int -> Maybe Int -> Maybe Int -> Maybe String
checkCapacityConfig BasicConfig {
    places,
    transitions,
    atLeastActive,
    maxTokensPerPlace,
    maxFlowPerEdge
    }
  maxCapacity
  minNewArrowsWithComplement
  oneMinCapacity
  | maxCapacity < maxFlowPerEdge
  = Just "'maxCapacity' can not be too low for flow weights."
  | maxCapacity < maxTokensPerPlace
  = Just "The starting tokens can not exceed 'maxCapacity'."
  | atLeastActive == 0
  = Just "At least one transition has to be activated."
  | otherwise
  = case minNewArrowsWithComplement of
      Just minNew
        | minNew <= 0
        -> Just "At least one flow has to be connected to a complement place."
        | minNew > 2 * transitions * places
        -> Just "'minNewArrowsWithComplement' is set unreasonably high, given the number of transitions."
      _ -> case oneMinCapacity of
          Just oneMin
            | oneMin <= 0
            -> Just "'oneMinCapacity' has to be positive."
            | oneMin > maxCapacity
            -> Just "'oneMinCapacity' can not be higher than 'maxCapacity'."
          _ -> Nothing

defaultCapacityInstance :: CapacityInstance
defaultCapacityInstance = CapacityInstance {
  drawWith = DrawSettings {
    withPlaceNames = True,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = ActivatedTransitions [Transition 1, Transition 2],
  originalNet = PetriLike {
    allNodes = M.fromList [
      ("s1",CapacityPlace {initial = 0, capacity = 0, flowIn = M.empty, flowOut = M.empty}),
      ("s2",CapacityPlace {initial = 0, capacity = 0, flowIn = M.empty, flowOut = M.empty}),
      ("s3",CapacityPlace {initial = 1, capacity = 3, flowIn = M.fromList [("t1",2),("t2",2)], flowOut = M.empty}),
      ("s4",CapacityPlace {initial = 2, capacity = 4, flowIn = M.empty, flowOut = M.fromList [("t1",1),("t2",2)]}),
      ("t1",CapacityTransition {flowIn = M.fromList [("s3",2),("s4",1)], flowOut = M.fromList [("s3",2)]}),
      ("t2",CapacityTransition {flowIn = M.fromList [("s3",2),("s4",2)], flowOut = M.fromList [("s3",2)]})
      ]
    },
  transformedNet = PetriLike {
    allNodes = M.fromList [
      ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",2),("t2",2)]}),
      ("s2",SimplePlace {initial = 2, flowOut = M.empty}),
      ("s3",SimplePlace {initial = 1, flowOut = M.empty}),
      ("s4",SimplePlace {initial = 2, flowOut = M.fromList [("t1",1),("t2",2)]}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",2)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s2",2),("s3",2)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
}
