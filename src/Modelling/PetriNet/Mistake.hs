{-# LANGUAGE ApplicativeDo #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.Mistake (
  checkFindMistakeConfig,
  defaultFindMistakeInstance,
  findMistake,
  findMistakeEvaluation,
  findMistakeGenerate,
  findMistakeSolution,
  findMistakeSyntax,
  findMistakeTask,
  parseConcurrency,
  petriNetFindMist,
  simpleFindMistakeTask,
  ) where

import qualified Modelling.PetriNet.Find          as F (showSolution)
import qualified Modelling.PetriNet.Types         as Find (
  FindMistakeConfig (..),
  )

import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Debug.Trace                      (trace)
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
  mistakeIsLegal,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignatureMistake,
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
  findInitial,
  findTaskInstance,
  toFindEvaluation,
  toFindSyntax,
  )
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Reach.Type (
  Transition (Transition),
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  DrawSettings (..),
  FindMistakeConfig (..),
  GraphConfig (..),
  MistakeConfig (..),
  Net (..),
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  SimplePetriNet,
  transitionPairShow,
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
  english,
  german,
  printSolutionAndAssert,
  translate,
  translations,
  unLangM,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Either                      (isLeft)
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance,
  )

simpleFindMistakeTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> FindInstance SimplePetriNet (Concurrent Transition)
  -> LangM m
simpleFindMistakeTask = findMistakeTask

findMistakeTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> FindInstance (p n String) (Concurrent Transition)
  -> LangM m
findMistakeTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net:"
    german "Betrachten Sie folgendes Petrinetz:"
  image
    $=<< renderWith path "concurrent" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english [iii|
      Which pair of transitions is concurrently activated
      under the initial marking?
      |]
    german [iii|
      Welches Paar von Transitionen ist unter der Startmarkierung
      nebenläufig aktiviert?
      |]
  paragraph $ do
    translate $ do
      english [iii|
        State your answer by giving a pair
        of concurrently activated transitions.
        #{" "}|]
      german [iii|
        Geben Sie Ihre Antwort durch Angabe eines Paars
        von nebenläufig aktivierten Transitionen an.
        #{" "}|]
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts = transitionPairShow findInitial
    code $ show ts
    translate $ do
      let (t1, t2) = bimap show show ts
      english [iii|
        #{" "}as answer would indicate that transitions #{t1} and #{t2}
        are concurrently activated under the initial marking.
        #{" "}|]
      german [iii|
        #{" "}als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2}
        unter der Startmarkierung nebenläufig aktiviert sind.
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

findMistakeSyntax
  :: OutputCapable m
  => FindInstance net (Concurrent Transition)
  -> (Transition, Transition)
  -> LangM' m ()
findMistakeSyntax = toFindSyntax False . numberOfTransitions

findMistakeEvaluation
  :: (Monad m, OutputCapable m)
  => FindInstance net (Concurrent Transition)
  -> (Transition, Transition)
  -> Rated m
findMistakeEvaluation task x = do
  let what = translations $ do
        english "are concurrently activated"
        german "sind nebenläufig aktiviert"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ toFindEvaluation what withSol concur x
  where
    concur = findMistakeSolution task
    withSol = F.showSolution task

findMistakeSolution :: FindInstance net (Concurrent a) -> (a, a)
findMistakeSolution task = concur
  where
    Concurrent concur = toFind task

findMistakeGenerate
  :: (MonadAlloy m, MonadThrow m, MonadIO m, Net p n)
  => FindMistakeConfig
  -> Int
  -> Int
  -> m (FindInstance (p n String) (Concurrent Transition))
findMistakeGenerate config segment seed = flip evalRandT (mkStdGen seed) $ do
  let alloyFile = petriNetFindMist config
  let fileName = "output.als"
  liftIO $ writeFile fileName alloyFile
  (d, c) <- trace "findMistake successful" <$> findMistake config segment
  gl <- oneOf $ graphLayouts gc
  c' <- lift $ traverse
     (parseWith parseTransitionPrec)
     c
  return $ FindInstance {
    drawFindWith   = DrawSettings {
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

findMistake
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => FindMistakeConfig
  -> Int
  -> RandT g m (p n String, Concurrent String)
findMistake = taskInstance findTaskInstance petriNetFindMist parseConcurrency Find.alloyConfig

petriNetFindMist :: FindMistakeConfig -> String
petriNetFindMist FindMistakeConfig{
  basicConfig,
  advConfig,
  changeConfig,
  mistakeConfig
  } = petriNetMistakeAlloy basicConfig changeConfig (Right advConfig) mistakeConfig

{-|
Generate code for Mistake PetriNet tasks
-}
petriNetMistakeAlloy
  :: BasicConfig
  -> ChangeConfig
  -> Either Bool AdvConfig
  -- ^ Right for find task; Left for pick task
  -> MistakeConfig
  -> String
petriNetMistakeAlloy basicC changeC specific mistakeC
  = [i|module PetriNetMist

#{modulePetriSignatureMistake}
#{either (const sigs) (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{mistakePredicateName}[#{defaultActiveTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{sourceTransitionConstraints}
  #{compConstraints}
  #{mistakeIsLegal mistakeC}

}

run #{mistakePredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
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
    sigs = signatures "given" (places basicC) (transitions basicC)
    t1 = transition1
    t2 = transition2

mistakePredicateName :: String
mistakePredicateName = "showMistake"

concurrencyTransition1 :: String
concurrencyTransition1 = skolemVariable mistakePredicateName transition1

concurrencyTransition2 :: String
concurrencyTransition2 = skolemVariable mistakePredicateName transition2

transition1 :: String
transition1 = "transition1"

transition2 :: String
transition2 = "transition2"

{-|
Parses the concurrency Skolem variables for singleton of transitions and returns
both as tuple.
It throws an error instead if unexpected behaviour occurs.
-}
parseConcurrency :: MonadThrow m => AlloyInstance -> m (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

checkFindMistakeConfig :: FindMistakeConfig -> Maybe String
checkFindMistakeConfig FindMistakeConfig {
  basicConfig,
  changeConfig,
  graphConfig
  }
  = checkConfigForFind basicConfig changeConfig graphConfig

defaultFindMistakeInstance :: FindInstance SimplePetriNet (Concurrent Transition)
defaultFindMistakeInstance = FindInstance {
  drawFindWith = DrawSettings {
    withPlaceNames = False,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = Concurrent (Transition 1,Transition 3),
  net = PetriLike {
    allNodes = M.fromList [
      ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",1),("t2",2),("t3",1),("s2",-3)]}),
      ("s2",SimplePlace {initial = 1, flowOut = M.empty}),
      ("s3",SimplePlace {initial = 1, flowOut = M.fromList [("t3",1)]}),
      ("s4",SimplePlace {initial = 0, flowOut = M.empty}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s3",1)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s2",1),("s4",2)]}),
      ("t3",SimpleTransition {flowOut = M.fromList [("s2",2)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
  }
