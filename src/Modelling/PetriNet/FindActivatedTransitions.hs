{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Modelling.PetriNet.FindActivatedTransitions (
  checkActivatedTransitionsConfig,
  checkFindActivatedTransitionsConfig,
  defaultFindActivatedTransitionsInstance,
  findActivatedTransitions,
  findActivatedTransitionsEvaluation,
  findActivatedTransitionsGenerate,
  findActivatedTransitionsSolution,
  findActivatedTransitionsSyntax,
  findActivatedTransitionsTask,
  parseActivatedTransitions,
  petriNetFindActivated,
  simpleFindActivatedTransitionsTask,
  ) where

import qualified Modelling.PetriNet.Find          as F (showSolution)
import qualified Modelling.PetriNet.Types         as Find (
  FindActivatedTransitionsConfig (..),
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
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitWidth,
  petriScopeMaxSeq,
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  )
import Modelling.PetriNet.Diagram (
  renderWith,
  )
import Modelling.PetriNet.Find (
  FindInstance (..),
  findInitialList,
  findTaskInstance,
  prohibitHideTransitionNames,
  toFindEvaluationList,
  )
import Modelling.PetriNet.Reach.Type (
  ShowTransition (ShowTransition),
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  ActivatedTransitions (ActivatedTransitions),
  AdvConfig,
  BasicConfig (..),
  ChangeConfig (..),
  DrawSettings (..),
  FindActivatedTransitionsConfig (..),
  GraphConfig (..),
  Net,
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  SimplePetriNet,
  checkBasicConfig,
  checkChangeConfig,
  transitionListShow,
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
import Data.Maybe                       (isNothing)
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance
  )

findActivatedTransitionsGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => FindActivatedTransitionsConfig
  -> Int
  -> Int
  -> m (FindInstance (p n String) (ActivatedTransitions Transition))
findActivatedTransitionsGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  (d, c) <- findActivatedTransitions config segment
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

simpleFindActivatedTransitionsTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> FindInstance SimplePetriNet (ActivatedTransitions Transition)
  -> LangM m
simpleFindActivatedTransitionsTask = findActivatedTransitionsTask

findActivatedTransitionsTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> FindInstance (p n String) (ActivatedTransitions Transition)
  -> LangM m
findActivatedTransitionsTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net:"
    german "Betrachten Sie folgendes Petrinetz:"
  image
    $=<< renderWith path "activatedTransition" (net task) (drawFindWith task)
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

findActivatedTransitionsSyntax
  :: OutputCapable m
  => FindInstance net (ActivatedTransitions Transition)
  -> [Transition]
  -> LangM' m ()
findActivatedTransitionsSyntax task transitions = do
  for_ transitions assertTransition
  pure ()
  where
    assert = continueOrAbort False
    assertTransition t = assert (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a transition of the given Petri net?"
      german $ t' ++ " ist eine Transition des gegebenen Petrinetzes?"
    isValidTransition (Transition x) = x >= 1 && x <= numberOfTransitions task

findActivatedTransitionsEvaluation
  :: (Monad m, OutputCapable m)
  => FindInstance net (ActivatedTransitions Transition)
  -> [Transition]
  -> Rated m
findActivatedTransitionsEvaluation task x = do
  let what = translations $ do
        english "are activated"
        german "sind aktiviert"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ toFindEvaluationList what withSol active x
  where
    active = findActivatedTransitionsSolution task
    withSol = F.showSolution task

findActivatedTransitionsSolution :: FindInstance net (ActivatedTransitions a) -> [a]
findActivatedTransitionsSolution task = active
  where
    ActivatedTransitions active = toFind task

findActivatedTransitions
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => FindActivatedTransitionsConfig
  -> Int
  -> RandT
    g
    m
    (p n String, ActivatedTransitions String)
findActivatedTransitions = taskInstance
  findTaskInstance
  petriNetFindActivated
  parseActivatedTransitions
  Find.alloyConfig

petriNetFindActivated :: FindActivatedTransitionsConfig -> String
petriNetFindActivated FindActivatedTransitionsConfig {
  basicConfig,
  advConfig,
  changeConfig,
  atMostActive
  }
  = petriNetActivatedTransitionsAlloy
    basicConfig
    changeConfig
    atMostActive
    advConfig

parseActivatedTransitions :: MonadThrow m => AlloyInstance -> m (ActivatedTransitions Object)
parseActivatedTransitions inst = do
  t <- unscopedSingleSig inst activatedTransitions ""
  pure $ ActivatedTransitions (Set.toList t)

petriNetActivatedTransitionsAlloy
  :: BasicConfig
  -> ChangeConfig
  -> Maybe Int
  -> AdvConfig
  -> String
petriNetActivatedTransitionsAlloy basicC changeC atMost advConfig
  = [i|module PetriNetFindActivatedTransitions

#{modulePetriSignature}
#{const modulePetriAdditions advConfig}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{activePredicateName}[#{activated} : set Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints True atMost activated basicC}
  #{compChange changeC}
  #{compAdvConstraints advConfig}

  #{activatedConstraint basicC atMost}
  no t : givenTransitions | activatedDefault[t]
}

run #{activePredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]
  where
    activated = skolemName
    activatedConstraint :: BasicConfig -> Maybe Int -> String
    activatedConstraint BasicConfig{ transitions, atLeastActive } atMostActive
      | atLeastActive == 0 && isNothing atMostActive
      = [i|theActivatedTransitions[#{activated}]|]
      | isNothing atMostActive
      = [i|\##{activated} =< #{transitions}|]
      | otherwise
      = "" -- because in all other cases already compBasicConstraints emits that constraint

activePredicateName :: String
activePredicateName = "showActiveTransition"

activatedTransitions :: String
activatedTransitions = skolemVariable activePredicateName skolemName

skolemName :: String
skolemName = "activatedTrans"

checkFindActivatedTransitionsConfig :: FindActivatedTransitionsConfig -> Maybe String
checkFindActivatedTransitionsConfig FindActivatedTransitionsConfig {
  basicConfig,
  changeConfig,
  atMostActive,
  graphConfig
  }
  = prohibitHideTransitionNames graphConfig
  <|> checkBasicConfig basicConfig
  <|> checkChangeConfig basicConfig changeConfig
  <|> checkActivatedTransitionsConfig basicConfig atMostActive

checkActivatedTransitionsConfig :: BasicConfig -> Maybe Int -> Maybe String
checkActivatedTransitionsConfig BasicConfig {
    atLeastActive,
    transitions
    }
  atMostActive =
      case atMostActive of
        Just atMost
          | atMost < 0
          -> Just "atMostActive must be non-negative."
          | atMost == transitions
          -> Just "When atMostActive equals the total number of transitions, it is redundant. Rather use atMostActive = 'Nothing' instead."
          | atLeastActive > atMost
          -> Just "atLeastActive must not be greater than atMostActive."
          | transitions < atMost
          -> Just "There must be at least as many transitions as atMostActive."
        _ -> Nothing

defaultFindActivatedTransitionsInstance :: FindInstance SimplePetriNet (ActivatedTransitions Transition)
defaultFindActivatedTransitionsInstance = FindInstance {
  drawFindWith = DrawSettings {
    withPlaceNames = False,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = ActivatedTransitions [Transition 1, Transition 2],
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
