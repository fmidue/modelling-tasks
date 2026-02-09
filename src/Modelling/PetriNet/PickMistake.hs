{-# LANGUAGE ApplicativeDo #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}

module Modelling.PetriNet.PickMistake (
  checkMistakeConfig,
  checkPickMistakeConfig,
  defaultPickMistakeInstance,
  petriNetPickMistake,
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
  signatures,
  taskInstance,
  )
import Modelling.PetriNet.Pick (
  PickInstance (..),
  checkConfigForPick,
  pickGenerate,
  pickTaskInstance,
  renderPick,
  wrong,           -- note that "wrong" in the context of the current module means
  wrongInstances,  -- "not having been infused with a mistake"
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
  basicConfigBitWidthInput,
  petriScopeBitWidth,
  )

import Control.Applicative              ((<|>))
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.OutputCapable.Blocks (
  ExtraText (..),
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
import Control.Monad.Trans              (MonadTrans (lift))
import Data.Data                        (Data, Typeable)
import Data.GraphViz.Commands           (GraphvizCommand (Fdp))
import Data.Functor.Const               (Const(..))
import Data.String.Interpolate          (i, iii)

pickMistakeGenerate
  :: (MonadAlloy m, MonadCatch m, MonadDiagrams m, MonadGraphviz m, Net p n)
  => PickMistakeConfig
  -> Int
  -> Int
  -> m (PickInstance (p n String))
pickMistakeGenerate = pickGenerate pickMistake gc ud ws et
  where
    gc = Pick.graphConfig
    ud = Pick.useDifferentGraphLayouts
    ws = Pick.printSolution
    et = Pick.extraText

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
    Data (n String),
    Data (p n String),
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m,
    Typeable n,
    Typeable p
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
    $=<< renderPick path task
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
  hoveringInformation True
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
  (\f -> lift . pickTaskInstance f)
  petriNetPickMistake
  (\_ -> return (Const ()))
  Pick.alloyConfig


petriNetPickMistake :: PickMistakeConfig -> String
petriNetPickMistake PickMistakeConfig{
  basicConfig,
  changeConfig,
  mistakeConfig
  } =
  petriNetPickMistakeAlloy
    basicConfig
    changeConfig
    mistakeConfig

{-|
Generate code for PetriNet mistake tasks
-}
petriNetPickMistakeAlloy
  :: BasicConfig
  -> ChangeConfig
  -> MistakeConfig
  -> String
petriNetPickMistakeAlloy basicC changeC mistakeC
  = [i|module PetriNetPickMistake

#{modulePetriSignature}
#{sigs}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{mistakePredicateName} {
  #{compBasicConstraints False Nothing undefined basicC}
  #{pickMistakeConstraints mistakeC}
  #{compChange changeC}
  #{defaultConstraints undefined basicC}
  #{prohibitSelfLoops mistakeC}
}

run #{mistakePredicateName} for #{petriScopeBitWidth (basicConfigBitWidthInput basicC)} Int
|]
  where
    sigs = signatures "given" (places basicC) (transitions basicC)
    prohibitSelfLoops :: MistakeConfig -> String
    prohibitSelfLoops MistakeConfig{ canHaveTransitionToTransition, canHavePlaceToPlace }
      | canHaveTransitionToTransition && canHavePlaceToPlace
      =  [i|all n : Nodes | no n.flow[n]|]
      | canHaveTransitionToTransition
      =  [i|all n : Transitions | no n.flow[n]|]
      | canHavePlaceToPlace
      =  [i|all n : Places | no n.flow[n]|]
      | otherwise
      = ""

mistakePredicateName :: String
mistakePredicateName = "showMistake"

pickMistakeConstraints :: MistakeConfig -> String
pickMistakeConstraints MistakeConfig
                { canHaveNegativeWeight, canHaveTransitionToTransition, canHavePlaceToPlace
                } = falseInput
  where
    input :: [(Bool, String)]
    input = [(canHaveNegativeWeight, "all w : Nodes.flow[Nodes] | w > 0"),
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
    flowChangeOverall
    }
  MistakeConfig {
    canHaveNegativeWeight,
    canHaveTransitionToTransition,
    canHavePlaceToPlace
    }
  | not (canHaveNegativeWeight || canHaveTransitionToTransition || canHavePlaceToPlace)
  = Just "At least one mistake must be enabled."
  | atLeastActive /= 0
  = Just "atLeastActive has to be 0 in this task type."
  | canHaveTransitionToTransition && transitions < 2
  = Just "At least two transitions are required for transition mistakes."
  | canHavePlaceToPlace && places < 2
  = Just "At least two places are required for place mistakes."
  | flowChangeOverall < 1
  = Just "flowChangeOverall must be at least 1 for mistakes."
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
  showSolution = False,
  addText = NoExtraText
  }
