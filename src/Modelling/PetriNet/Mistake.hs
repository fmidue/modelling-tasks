{-# LANGUAGE ApplicativeDo #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.Mistake (
  checkPickPossibleMistakeConfig,
  defaultPickMistakeInstance,
  parseMistake,
  petriNetPickMist,
  pickMistake,
  pickMistakeGenerate,
  pickMistakeTask,
  simplePickMistakeTask,
  ) where

import qualified Modelling.PetriNet.Types         as Pick (
  PickPossibleMistakeConfig (..),
  )

import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common       (Object)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Alloy (
  compBasicConstraints,
  compChange,
  defaultConstraints,
  mistakeConstraints,
  moduleHelpers,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitWidth,
  petriScopeMaxSeq,
  skolemVariable,
  taskInstance,
  unscopedSingleSig,
  )
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Pick (
  PickInstance (..),
  checkConfigForPick,
  pickGenerate,
  pickTaskInstance,
  renderPick,
  wrong,
  wrongInstances,
  )
import Modelling.PetriNet.Types         (
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  DrawSettings (..),
  PossibleMistakeConfig,
  Net (..),
  PetriLike (PetriLike, allNodes),
  PickPossibleMistakeConfig (..),
  SimpleNode (..),
  SimplePetriNet,
  )

import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  translate,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  )
import Data.GraphViz.Commands           (GraphvizCommand (Fdp))
import Data.String.Interpolate          (i, iii)
import Language.Alloy.Call (
  AlloyInstance,
  )

pickMistakeGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => PickPossibleMistakeConfig
  -> Int
  -> Int
  -> m (PickInstance (p n String))
pickMistakeGenerate = pickGenerate pickMistake gc ud ws
  where
    gc = Pick.graphConfig
    ud = Pick.useDifferentGraphLayouts
    ws = Pick.printSolution

simplePickMistakeTask
  :: (MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> PickInstance SimplePetriNet
  -> LangM m
simplePickMistakeTask = pickMistakeTask

pickMistakeTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> PickInstance (p n String)
  -> LangM m
pickMistakeTask path task = do
  paragraph $ translate $ do
    english [iii|
      Which of the following Petri nets is "illegal" meaning it violates fundamental constraints?
      |]
    german [iii|
      Welches dieser Petri-Netze ist "illegal", das heißt, es verletzt grundlegende Bedingungen?
      |]
  images show snd
    $=<< renderPick path "mistake" task
  paragraph $ translate $ do
    english [iii|
      State your answer by giving the number of the Petri net
      that is incorrect.
      #{" "}|]
    german [iii|
      Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzes an,
      das inkorrekt ist.
      #{" "}|]
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [iii|
        #{" "}as answer would indicate that Petri net 1 is "illegal" (and the other Petri
        #{if plural then "nets are valid" else "net is valid"}).
        |]
      german $ [iii|
        #{" "}als Antwort würde bedeuten, dass Petri-Netz 1
        "illegal" ist, während
        #{" "}
        |]
        ++ (if plural
            then "die anderen Petri-Netze gültig sind"
            else "das andere Petri-Netz gültig ist")
    pure ()
  paragraph hoveringInformation
  pure ()

pickMistake
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => PickPossibleMistakeConfig
  -> Int
  -> RandT
    g
    m
    [(p n String, Maybe (Concurrent String))]
pickMistake = taskInstance
  pickTaskInstance
  petriNetPickMist
  parseMistake
  Pick.alloyConfig

petriNetPickMist :: PickPossibleMistakeConfig -> String
petriNetPickMist PickPossibleMistakeConfig{
  basicConfig,
  changeConfig,
  possibleMistakeConfig
  } =
  petriNetMistakeAlloy
    basicConfig
    changeConfig
    possibleMistakeConfig

{-|
Generate code for PetriNet mistake tasks
-}
petriNetMistakeAlloy
  :: BasicConfig
  -> ChangeConfig
  -> PossibleMistakeConfig
  -> String
petriNetMistakeAlloy basicC changeC mistakeC
  = [i|module PetriNetMistake

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{mistakePredicateName}[#{defaultActiveTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints False activated basicC}
  #{compChange changeC}
  #{sourceTransitionConstraints}
  #{sinkTransitionConstraints}
  #{compConstraints}
  #{mistakeConstraints mistakeC}
}

run #{mistakePredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActiveTrans"
    sourceTransitionConstraints :: String
    sourceTransitionConstraints = [i|
  no t : givenTransitions | no givenPlaces.flow[t]
  no t : Transitions | sourceTransitions[t]|]
    sinkTransitionConstraints :: String
    sinkTransitionConstraints = "no t : Transitions | sinkTransitions[t]"
    defaultActiveTrans :: String
    defaultActiveTrans = [i|#{activatedDefault} : set givenTransitions,|]
    compConstraints = defaultConstraints activatedDefault basicC
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
parseMistake :: MonadThrow m => AlloyInstance -> m (Concurrent Object)
parseMistake inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

checkPickPossibleMistakeConfig :: PickPossibleMistakeConfig -> Maybe String
checkPickPossibleMistakeConfig PickPossibleMistakeConfig {
  basicConfig,
  changeConfig,
  graphConfig,
  useDifferentGraphLayouts
  }
  = checkConfigForPick
    useDifferentGraphLayouts
    wrong
    basicConfig
    changeConfig
    graphConfig

defaultPickMistakeInstance :: PickInstance SimplePetriNet
defaultPickMistakeInstance = PickInstance {
  nets = M.fromList [
    (1,(False,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",SimplePlace {initial = 1, flowOut = M.fromList [("t1",2),("t2",1),("t3",1)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = M.fromList [("t1",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = M.empty}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("s4",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = False,
        withSvgHighlighting = True,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      ))),
    (2,(True,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",SimplePlace {initial = 2, flowOut = M.fromList [("t1",2),("t2",1),("s2",1)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 0, flowOut = M.fromList [("t1",1)]}),
          ("s4",SimplePlace {initial = 2, flowOut = M.fromList [("t2",-2)]}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("s1",1),("s4",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",-1)]})
          ]
        },
      DrawSettings {
        withPlaceNames = False,
        withSvgHighlighting = True,
        withTransitionNames = False,
        with1Weights = False,
        withGraphvizCommand = Fdp
        }
      )))
    ],
  showSolution = False
  }
