{-# LANGUAGE ApplicativeDo #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.Mistake (
  checkPickMistakeConfig,
  defaultPickMistakeInstance,
  mistakeConstraints,
  petriNetPickMist,
  pickMistake,
  pickMistakeGenerate,
  pickMistakeTask,
  simplePickMistakeTask,
  ) where

import qualified Modelling.PetriNet.Types         as Pick (
  PickMistakeConfig (..),
  )

import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Alloy (
  compBasicConstraints,
  compChange,
  defaultConstraints,
  moduleHelpers,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitWidth,
  petriScopeMaxSeq,
  taskInstance,
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
  ChangeConfig (..),
  DrawSettings (..),
  MistakeConfig (..),
  Net (..),
  PetriLike (PetriLike, allNodes),
  PickMistakeConfig (..),
  SimpleNode (..),
  SimplePetriNet,
  )

import Control.Applicative              ((<|>))
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
import Data.Functor.Const               (Const(..))
import Data.String.Interpolate          (i, iii)

pickMistakeGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => PickMistakeConfig
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
      Which of the following Petri net candidates is not correctly formed?
      |]
    german [iii|
      Welcher der folgenden Petrinetzkandidaten ist nicht korrekt geformt?
      |]
  images show snd
    $=<< renderPick path "mistake" task
  paragraph $ translate $ do
    english [iii|
      State your answer by giving the number of the Petri net candidate
      that is syntactically incorrect.
      #{" "}|]
    german [iii|
      Geben Sie Ihre Antwort durch Angabe der Nummer des Petrinetzkandidaten an,
      der syntaktisch inkorrekt ist.
      #{" "}|]
  let plural = wrongInstances task > 1
  paragraph $ do
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    code "1"
    translate $ do
      english [iii|
        #{" "}as answer would indicate that Petri net candidate 1 is incorrect (and the other
        #{if plural then "ones are" else "one is"} at least syntactically correct).
        |]
      german $ [iii|
        #{" "}als Antwort würde bedeuten, dass Petrinetzkandidat 1
        inkorrekt ist, während
        #{" "}
        |]
        ++ (if plural
            then "die anderen zumindest syntaktisch korrekt sind."
            else "der andere zumindest syntaktisch korrekt ist.")
    pure ()
  paragraph hoveringInformation
  pure ()

pickMistake
  :: (MonadAlloy m, MonadThrow m, Net p n, RandomGen g)
  => PickMistakeConfig
  -> Int
  -> RandT
    g
    m
    [(p n String, Maybe (Const () String))]
pickMistake = taskInstance
  pickTaskInstance
  petriNetPickMist
  (\_ -> return (Const ()))
  Pick.alloyConfig

petriNetPickMist :: PickMistakeConfig -> String
petriNetPickMist PickMistakeConfig{
  basicConfig,
  changeConfig,
  mistakeConfig
  } =
  petriNetMistakeAlloy
    basicConfig
    changeConfig
    mistakeConfig

{-|
Generate code for PetriNet mistake tasks
-}
petriNetMistakeAlloy
  :: BasicConfig
  -> ChangeConfig
  -> MistakeConfig
  -> String
petriNetMistakeAlloy basicC changeC mistakeC
  = [i|module PetriNetMistake

#{modulePetriSignature}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{mistakePredicateName} {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints False undefined basicC}
  #{mistakeConstraints mistakeC}
  #{compChange changeC}
  #{defaultConstraints undefined basicC}
}

run #{mistakePredicateName} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitWidth basicC} Int
|]

mistakePredicateName :: String
mistakePredicateName = "showMistake"

mistakeConstraints :: MistakeConfig -> String
mistakeConstraints MistakeConfig
                { canHaveNegativeTokenCost, canHaveTransitionToTransition, canHavePlaceToPlace
                } = falseInput
  where
    input :: [(Bool, String)]
    input = [(canHaveNegativeTokenCost, "all w : Nodes.flow[Nodes] | w > 0"),
             (canHaveTransitionToTransition, "Transitions.flow.Int in Places"),
             (canHavePlaceToPlace, "Places.flow.Int in Transitions")]
    falseMistakes = map snd (filter (not . fst) input)

    falseInput :: String
    falseInput =
      case falseMistakes of
        []     -> ""
        (x:xs) -> unlines (x : map ("  " ++) xs)

checkPickMistakeConfig :: PickMistakeConfig -> Maybe String
checkPickMistakeConfig PickMistakeConfig {
  basicConfig,
  changeConfig,
  mistakeConfig,
  graphConfig,
  useDifferentGraphLayouts
  }
  = checkConfigForPick
    useDifferentGraphLayouts
    wrong
    basicConfig
    changeConfig
    graphConfig
  <|> checkMistakeConfig basicConfig changeConfig mistakeConfig

checkMistakeConfig :: BasicConfig -> ChangeConfig -> MistakeConfig -> Maybe String
checkMistakeConfig BasicConfig {
    places,
    transitions,
    atLeastActive
    }
  ChangeConfig {
    flowChangeOverall,
    maxFlowChangePerEdge
    }
  MistakeConfig {
    canHaveNegativeTokenCost,
    canHaveTransitionToTransition,
    canHavePlaceToPlace
    }
  | not (canHaveNegativeTokenCost || canHaveTransitionToTransition || canHavePlaceToPlace)
  = Just "At least one mistake must be enabled."
  | atLeastActive /= 0
  = Just "atLeastActive has to be 0 in this task type."
  | canHaveTransitionToTransition && transitions < 2
  = Just "At least two transitions are required for transition mistakes."
  | canHavePlaceToPlace && places < 2
  = Just "At least two places are required for place mistakes."
  | (canHaveTransitionToTransition || canHavePlaceToPlace) && flowChangeOverall < 1
  = Just "flowChangeOverall must be at least 1 for mistakes."
  | (canHaveTransitionToTransition && canHaveTransitionToTransition && canHavePlaceToPlace) && flowChangeOverall < 2
  = Just "flowChangeOverall must be greater than 1 for all mistakes."
  | maxFlowChangePerEdge < 1
  = Just "maxFlowChangePerEdge must be at least 1 for mistakes to appear."
  | otherwise
  = Nothing

defaultPickMistakeInstance :: PickInstance SimplePetriNet
defaultPickMistakeInstance = PickInstance {
  nets = M.fromList [
    (1,(False,(
      PetriLike {
        allNodes = M.fromList [
          ("s1",SimplePlace {initial = 3, flowOut = M.fromList [("t1",1),("t3",2)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 1, flowOut = M.fromList [("t2",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = M.fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("s1",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s4",2)]})
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
          ("s1",SimplePlace {initial = 3, flowOut = M.fromList [("t1",-1),("t3",2)]}),
          ("s2",SimplePlace {initial = 0, flowOut = M.empty}),
          ("s3",SimplePlace {initial = 1, flowOut = M.fromList [("t2",1)]}),
          ("s4",SimplePlace {initial = 1, flowOut = M.fromList [("t2",1)]}),
          ("t1",SimpleTransition {flowOut = M.fromList [("s2",1),("s3",1)]}),
          ("t2",SimpleTransition {flowOut = M.fromList [("t1",1)]}),
          ("t3",SimpleTransition {flowOut = M.fromList [("s4",2)]})
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
