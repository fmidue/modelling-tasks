{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.ActiveTransition (
  checkActiveTransitionConfig,
  checkFindActiveTransitionConfig,
  defaultFindActiveTransitionInstance,
  findActiveTransition,
  findActiveTransitionEvaluation,
  findActiveTransitionGenerate,
  findActiveTransitionSolution,
  findActiveTransitionTask,
  parseActiveTransition,
  petriNetFindActive,
  simpleFindActiveTransitionTask,
  ) where

import qualified Modelling.PetriNet.Find          as F (showSolution)
import qualified Modelling.PetriNet.Types         as Find (
  FindActiveTransitionConfig (..),
  )
import qualified Data.Map                         as M (
  empty,
  fromList,
  )
import qualified Data.Set                         as Set (
  toList,
  )

import Capabilities.Alloy               (MonadAlloy)
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
  compBasicConstraints,
  compChange,
  defaultConstraints,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitWidth,
  petriScopeMaxSeq,
  signatures,
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  )
import Modelling.PetriNet.Diagram (
  renderWith,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  checkConfigForFind,
  findInitialList,
  findTaskInstance,
  toFindEvaluationList,
  )
import Modelling.PetriNet.Reach.Type (
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  ActiveTransition (ActiveTransition),
  AdvConfig,
  BasicConfig (..),
  ChangeConfig (..),
  ActiveTransitionConfig (..),
  DrawSettings (..),
  FindActiveTransitionConfig (..),
  GraphConfig (..),
  Net,
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  SimplePetriNet,
  transitionListShow,
  )

import Control.Applicative              ((<|>))
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
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
import Data.Either                      (isLeft)
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance
  )

findActiveTransitionGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => FindActiveTransitionConfig
  -> Int
  -> Int
  -> m (FindInstance (p n String) (ActiveTransition Transition))
findActiveTransitionGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findActiveTransition config segment
  gl <- oneOf $ graphLayouts gc
  c' <- lift $ traverse
    (parseWith parseTransitionPrec)
    c
  return $ FindInstance {
    drawFindWith = DrawSettings {
      withPlaceNames = not $ hidePlaceNames gc,
      withSvgHighlighting = True,
      withTransitionNames = not $ hideTransitionNames gc,
      with1Weights = not $ hideWeight1 gc,
      withGraphvizCommand = gl
      },
    toFind = c',
    net = d,
    numberOfPlaces = places bc,
    numberOfTransitions = transitions bc,
    showSolution = Find.printSolution config
    }
  where
    bc = Find.basicConfig config
    gc = Find.graphConfig config

simpleFindActiveTransitionTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> FindInstance SimplePetriNet (ActiveTransition Transition)
  -> LangM m
simpleFindActiveTransitionTask = findActiveTransitionTask

findActiveTransitionTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> FindInstance (p n String) (ActiveTransition Transition)
  -> LangM m
findActiveTransitionTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net:"
    german "Betrachten Sie folgendes Petrinetz:"
  image
    $=<< renderWith path "activeTransition" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english [iii|
      Which transitions are activated
      under the initial marking?
      |]
    german [iii|
      Welche Transitionen sind unter der Startmarkierung aktiviert?
      |]
  paragraph $ do
    translate $ do
      english [iii|
        State your answer by giving a list of activated transitions.
        #{" "}|]
      german [iii|
        Geben Sie Ihre Antwort durch Angabe einer Liste
        von aktivierten Transitionen an.
        #{" "}|]
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = transitionListShow findInitialList
    code $ show ts
    translate $ do
      let ta = map show findInitialList
      english [iii|
        #{" "}as answer would indicate that transitions #{ta}
        are activated under the initial marking.
        #{" "}|]
      german [iii|
        #{" "}als Antwort würde bedeuten, dass Transitionen #{ta}
        unter der Startmarkierung aktiviert sind.
        #{" "}|]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german [iii|
        Die Reihenfolge der Transitionen innerhalb
        des Paars spielt hierbei keine Rolle.
        |]

    pure ()
  paragraph hoveringInformation
  pure ()

findActiveTransitionEvaluation
  :: (Monad m, OutputCapable m)
  => FindInstance net (ActiveTransition Transition)
  -> [Transition]
  -> Rated m
findActiveTransitionEvaluation task x = do
  let what = translations $ do
        english "are activated"
        german "sind aktiviert"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ toFindEvaluationList what withSol active x
  where
    active = findActiveTransitionSolution task
    withSol = F.showSolution task

findActiveTransitionSolution :: FindInstance net (ActiveTransition a) -> [a]
findActiveTransitionSolution task = active
  where
    ActiveTransition active = toFind task

findActiveTransition
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => FindActiveTransitionConfig
  -> Int
  -> RandT
    g
    m
    (p n String, ActiveTransition String)
findActiveTransition = taskInstance
  findTaskInstance
  petriNetFindActive
  parseActiveTransition
  Find.alloyConfig

petriNetFindActive :: FindActiveTransitionConfig -> String
petriNetFindActive FindActiveTransitionConfig {
  basicConfig,
  advConfig,
  changeConfig,
  activeTransitionConfig
  }
  = petriNetActiveTransitionAlloy
    basicConfig
    changeConfig
    activeTransitionConfig
    $ Right advConfig

parseActiveTransition :: MonadThrow m => AlloyInstance -> m (ActiveTransition Object)
parseActiveTransition inst = do
  t <- unscopedSingleSig inst activeTransition1 ""
  pure $ ActiveTransition (Set.toList t)

petriNetActiveTransitionAlloy
  :: BasicConfig
  -> ChangeConfig
  -> ActiveTransitionConfig
  -> Either Bool AdvConfig
  -- ^ Right for find task; Left for pick task
  -> String
petriNetActiveTransitionAlloy basicC changeC activeC specific
  = [i|module PetriNetActiveTransition

#{modulePetriSignature}
#{either (const sigs) (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{activePredicateName}[#{defaultActiveTrans}#{activated} : set Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints True activated basicC}
  #{compChange changeC}
  #{sourceTransitionConstraints}
  #{compConstraints}

  no t : givenTransitions | activatedDefault[t]
  #{maxActivatedTrans activeC}
}

run #{activePredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActiveTrans"
    compConstraints = either
      (const $ defaultConstraints activatedDefault basicC)
      compAdvConstraints
      specific
    sourceTransitionConstraints
      | Left True <- specific = [i|
  no t : givenTransitions | no givenPlaces.flow[t]
  no t : Transitions | sourceTransitions[t]|]
      | otherwise = ""
    defaultActiveTrans
      | isLeft specific    = [i|#{activatedDefault} : set givenTransitions,|]
      | otherwise          = ""
    maxActivatedTrans :: ActiveTransitionConfig -> String
    maxActivatedTrans ActiveTransitionConfig {atMostActive}
      = "#" ++ [i|#{activated} <= #{atMostActive}|]

    sigs = signatures "given" (places basicC) (transitions basicC)

activePredicateName :: String
activePredicateName = "showActiveTransition"

activeTransition1 :: String
activeTransition1 = skolemVariable activePredicateName transition1

transition1 :: String
transition1 = "activatedTrans"

checkFindActiveTransitionConfig :: FindActiveTransitionConfig -> Maybe String
checkFindActiveTransitionConfig FindActiveTransitionConfig {
  basicConfig,
  changeConfig,
  activeTransitionConfig,
  graphConfig
  }
  = checkConfigForFind basicConfig changeConfig graphConfig
  <|> checkActiveTransitionConfig basicConfig changeConfig activeTransitionConfig

checkActiveTransitionConfig :: BasicConfig -> ChangeConfig -> ActiveTransitionConfig -> Maybe String
checkActiveTransitionConfig BasicConfig {
    transitions,
    atLeastActive
    }
  ChangeConfig {

    }
  ActiveTransitionConfig {
    atMostActive
    }
  | atLeastActive >= atMostActive
  = Just "atLeastActive must be less than atMostActive."
  | transitions <= atLeastActive
  = Just "There must be at least as many transitions as atLeastActive."
  | transitions <= atMostActive
  = Just "There must be at least as many transitions as atMostActive."
  | otherwise
  = Nothing

defaultFindActiveTransitionInstance :: FindInstance SimplePetriNet (ActiveTransition Transition)
defaultFindActiveTransitionInstance = FindInstance {
  drawFindWith = DrawSettings {
    withPlaceNames = False,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = ActiveTransition [Transition 1, Transition 2],
  net = PetriLike {
    allNodes = M.fromList [
      ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",1),("t2",1)]}),
      ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
      ("s3",SimplePlace {initial = 0, flowOut = M.empty}),
      ("s4",SimplePlace {initial = 1, flowOut = M.fromList [("t1",1),("t3",2)]}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s2",2),("s3",2)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s3",1)]}),
      ("t3",SimpleTransition {flowOut = M.fromList [("s3",1)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
