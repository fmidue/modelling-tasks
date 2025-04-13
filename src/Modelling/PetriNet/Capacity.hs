{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Modelling.PetriNet.Capacity (
  CapacityInstance (..),
  capacityEvaluation,
  capacityGenerate,
  capacitySyntax,
  capacityTask,
  checkCapacityConfigs,
  defaultCapacityInstance,
  petriNetFindCapacity,
  parseCapacityPrec,
  simpleCapacityTask,
  ) where

import qualified Modelling.PetriNet.Reach.Type    as Reach (
  Place(..),
  Transition(..),
  )
import qualified Modelling.PetriNet.Types         as Find (
  AlloyConfig (maxInstances, timeout),
  CapacityConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Pick (
  CapacityConfig (..),
  )
import qualified Modelling.PetriNet.Types         as Types (
  NodeC(..)
  )
import qualified Data.Map                         as M (
  empty,
  fromList,
  )

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  TaskGenerationException (NoInstanceAvailable),
  Object,
  oneOf,
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
  )
import Modelling.PetriNet.Diagram (
  getDefaultNet,
  getNet,
  renderWith,
  )
import Modelling.PetriNet.Find (
  prohibitHidePlaceNames,
  prohibitHideTransitionNames,
  prohibitPatchworkRenderer,
  toFindEvaluationList,
  )
import Modelling.PetriNet.FindActivatedTransitions (
  checkActivatedTransitionsConfig,
  )
import Modelling.PetriNet.Parser (
  parseChange,
  )
import Modelling.PetriNet.Reach.Type (
  ShowTransition (ShowTransition),
  Transition,
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  ActivatedTransitions (ActivatedTransitions),
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  Capacity (..),
  CapacityConfig (..),
  DrawSettings (..),
  GraphConfig (..),
  Net,
  NodeC (..),
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  SimplePetriNet,
  CapacityNode (..),
  basicConfigBitWidthInput,
  checkActivatedSourceConfig,
  checkBasicConfig,
  petriScopeBitWidth,
  )

import Control.Applicative              ((<|>))
import Control.Monad                    (void, when)
import Control.Monad.Catch              (MonadThrow, MonadThrow (throwM))
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
import Text.Parsec (
  char,
  optionMaybe,
  sepBy,
  spaces,
  )
import Text.Parsec.Char                 (digit)
import Text.Parsec.Combinator           (many1)
import Text.Parsec.String               (Parser)


data CapacityInstance a = CapacityInstance {
  drawWith :: !DrawSettings,
  toFind :: !(PetriChangeList String),
  originalNet :: !(PetriLike CapacityNode String),
  transformedNet :: !a,
  numberOfPlaces :: !Int,
  numberOfTransitions :: !Int,
  showSolution :: !Bool
  }
  deriving (Show)

capacityGenerate
  :: (MonadAlloy m, MonadThrow m, Net p n)
  => CapacityConfig
  -> Int
  -> Int
  -> m (CapacityInstance (p n String))
capacityGenerate config seed segment =
  flip evalRandT (mkStdGen seed) $ do
    gl <- oneOf $ graphLayouts gc

    (original, transformed, condition) <- combinedCapacity petriNetFindCapacity Find.alloyConfig config segment

    return $ CapacityInstance
      { drawWith = DrawSettings
          { withPlaceNames = not $ hidePlaceNames gc
          , withSvgHighlighting = True
          , withTransitionNames = not $ hideTransitionNames gc
          , with1Weights = not $ hideWeight1 gc
          , withGraphvizCommand = gl
          }
      , toFind = condition
      , originalNet = original
      , transformedNet = transformed
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
  -> CapacityInstance SimplePetriNet
  -> LangM m
simpleCapacityTask = capacityTask

capacityTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Net p n,
    OutputCapable m
    )
  => FilePath
  -> CapacityInstance (p n String)
  -> LangM m
capacityTask path task = do
  paragraph $ translate $ do
    english "Consider the following Petri net with capacities:"
    german "Betrachten Sie folgendes Petrinetz mit Kapazitäten:"
  image
    $=<< renderWith path "capacityTask" (originalNet task) (drawWith task)
  image
    $=<< renderWith path "capacitySolution" (transformedNet task) (drawWith task)
  paragraph $ do
    translate $ do
      english [iii|
        Given the isolated Places. With how many tokens and how should they be connected to Transitions so that the
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
    translate $ do
      english [i|Stating |]
      german [i|Die Angabe von |]
    let ts :: ([(String, Int)], [(String, String, Int)])
        ts = ([("s1", 2), ("s2", 0)], [("t1", "s1", 1), ("t2", "s1", 1), ("s2", "t2", 2)])
    code $ show ts
    translate $ do
      english ("as answer would indicate that there are two complement places - p1 with 2 tokens and p2 with 0 tokens - and t1 points to s1 with a weight of 1, " ++
               "t2 points to s1 with a weight of 1 and s2 connects to t2 with a weight of 2.")
      german ("als Antwort würde bedeuten, dass es zwei Komplementstellen gibt - p1 mit 2 Token und p2 mit 0 Token - und t1 zeigt auf s1 mit einem Gewicht von 1, " ++
             "t2 zeigt auf s1 mit einem Gewicht von 1, und s2 ist mit t2 mit einem Gewicht von 2 verbunden.")
    translate $ do
      english "The order of tupels within the lists does not matter here."
      german "Die Reihenfolge der Tupel innerhalb der Listen spielt hierbei keine Rolle."
    pure ()
  paragraph hoveringInformation
  pure ()

capacitySyntax
  :: OutputCapable m
  => CapacityInstance net
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
    isValidTransition (Reach.Transition x) = x >= 1 && x <= numberOfTransitions task

capacityEvaluation
  :: (Monad m, OutputCapable m)
  => CapacityInstance net
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

capacitySolution :: CapacityInstance net -> [Transition]
capacitySolution task = active
  where
    ActivatedTransitions active = toFind task

combinedCapacity
  :: (MonadThrow m, RandomGen g, MonadAlloy m, Net p n)
  => (config -> String)
  -> (config -> AlloyConfig)
  -> config
  -> Int
  -> RandT g m (PetriLike CapacityNode String, p n String, PetriChangeList String)
combinedCapacity alloyF alloyC config segment = do
  let is = Find.maxInstances (alloyC config)
  list <- getInstances is (Find.timeout $ alloyC config) (alloyF config)
  when (null $ drop segment list)
    $ throwM NoInstanceAvailable
  inst <- case fromIntegral <$> is of
    Nothing -> randomInstance list
    Just n -> do
      x <- randomInSegment segment n
      case drop x list of
        x':_ -> return x'
        []   -> randomInstance list
  first <- getDefaultNet (Just "capacity") inst
  (second, third) <- getNet (fmap toChangeList . parseChange) inst

  return (first, second, third)
  where
    randomInstance list = do
      n <- randomInSegment segment (1 + ((length list - segment - 1) `div` 4))
      return $ list !! n

petriNetFindCapacity :: CapacityConfig -> String
petriNetFindCapacity CapacityConfig {
  basicConfig,
  advConfig,
  maxCapacity,
  newArrowsWithComplement,
  oneMinCapacity,
  distractors,
  atMostActive
  }
  = petriNetFindCapacityAlloy
    basicConfig
    advConfig
    maxCapacity
    newArrowsWithComplement
    oneMinCapacity
    distractors
    atMostActive

parseCapacityPrec :: Int -> Parser Capacity
parseCapacityPrec _ = do
  spaces
  void $ char '('
  spaces
  places <- parsePlacesWithInts
  spaces
  void $ char ','
  spaces
  flows <- parseFlowTriples
  spaces
  void $ char ')'
  return (Capacity (places, flows))
  where
    parsePlacesWithInts =
      char '[' *> parsePlaceWithInt `sepBy` (spaces *> char ',' <* spaces) <* char ']'

    parsePlaceWithInt = do
      spaces
      void $ char '('
      spaces
      p <- parsePlacePrec 0
      spaces
      void $ char ','
      spaces
      n <- parseInt
      spaces
      void $ char ')'
      return (p, n)

    parseFlowTriples =
      char '[' *> parseFlowTriple `sepBy` (spaces *> char ',' <* spaces) <* char ']'

    parseFlowTriple = do
      spaces
      void $ char '('
      spaces
      a <- parseNodeC
      spaces
      void $ char ','
      spaces
      b <- parseNodeC
      spaces
      void $ char ','
      spaces
      n <- parseInt
      spaces
      void $ char ')'
      return (a, b, n)

    parseInt = read <$> many1 digit

    parseNodeC :: Parser NodeC
    parseNodeC = do
      tag <- optionMaybe (char 'p')
      case tag of
        Just _  -> do
          Reach.Place n <- parsePlacePrec 0
          return $ Types.Place (show n)
        Nothing -> do
          Reach.Transition n <- parseTransitionPrec 0
          return $ Types.Transition (show n)

petriNetFindCapacityAlloy
  :: BasicConfig
  -> AdvConfig
  -> Int
  -> (Int, Int)
  -> Int
  -> (Int, Int)
  -> Maybe Int
  -> String
petriNetFindCapacityAlloy basicC advConfig maxCapacity newArrowsWithComplement oneMinCapacity distractors atMostActive
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
  no addedTransitions
  no givenPlaces.tokenChange
  no givenPlaces.flowChange
  Transitions.flowChange.Int in addedPlaces
  addedPlaces.flowChange.Int in Transitions
}

pred #{capacityPredicateName}[#{activated} : set Transitions] {
  #{defaultConstraintsAtLeastZero}
  #{compAdvConstraints True advConfig}

  all t : Transitions, p : givenPlaces |
    let n = minus[t.flow[p], p.flow[t]] |
      n < 0 implies (t.flow[p.complement] = minus[0, n] and no p.complement.flow[t])
      else
      n > 0 implies (p.complement.flow[t] = n and no t.flow[p.complement])
      else
      no p.complement.flow[t] and no t.flow[p.complement]

  all p : placesWithCapacity, w : p.flow[Transitions] + Transitions.flow[p] | p.capacity >= w

  all p : addedPlaces | some p.flowChange.Int or some p.~(flowChange.Int)

  #{newArrowsWithComplementConstraints newArrowsWithComplement}
  #{oneMinCapacityConstraints oneMinCapacity}
  #{distractorsConstraints distractors}

}

run #{capacityPredicateName} for exactly #{places basicC} givenPlaces, exactly #{places basicC} addedPlaces,
exactly #{transitions basicC} Transitions, #{petriScopeBitWidth (basicConfigBitWidthInput basicC ++ [snd newArrowsWithComplement, maxCapacity])} Int
|]
  where
    activated = skolemName
    defaultConstraintsAtLeastZero =
      enforceConstraints True Nothing undefined (basicC { atLeastActive = 0 })
      ++
      "#" ++ activated ++ " >= " ++ show (atLeastActive basicC) ++ "\n"
      ++
      (case atMostActive of
        Nothing -> ""
        Just n  -> "#" ++ activated ++ " <= " ++ show n ++ "\n")
      ++
      "  theActivatedTransitions[" ++ activated ++ "]"
    newArrowsWithComplementConstraints (minNewArrowsMin, minNewArrowsMax) =
      "let newArrows = #flowChange | newArrows >= " ++ show minNewArrowsMin ++
      " and newArrows =< " ++ show minNewArrowsMax
    oneMinCapacityConstraints 1 = ""
    oneMinCapacityConstraints oneMinCap =
      [i|some p : placesWithCapacity | p.capacity >= #{oneMinCap}|]
    distractorsConstraints (distractorsMin, distractorsMax) =
      "let distractors = #{t : Transitions | activatedDefault[t] and t not in " ++ activated ++ "} |\n" ++
      "    distractors >= " ++ show distractorsMin ++ " and distractors =< " ++ show distractorsMax

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
  newArrowsWithComplement,
  oneMinCapacity,
  distractors,
  atMostActive,
  graphConfig
  }
  = prohibitHidePlaceNames graphConfig
  <|> prohibitHideTransitionNames graphConfig
  <|> checkBasicConfig [snd newArrowsWithComplement, maxCapacity] basicConfig
  <|> prohibitPatchworkRenderer graphConfig
  <|> checkActivatedSourceConfig basicConfig advConfig
  <|> checkCapacityConfig basicConfig maxCapacity newArrowsWithComplement oneMinCapacity distractors
  <|> checkActivatedTransitionsConfig basicConfig atMostActive

checkCapacityConfig :: BasicConfig -> Int -> (Int, Int) -> Int -> (Int, Int) -> Maybe String
checkCapacityConfig BasicConfig {
    places,
    transitions,
    atLeastActive,
    maxTokensPerPlace,
    maxFlowPerEdge
    }
  maxCapacity
  newArrowsWithComplement
  oneMinCapacity
  distractors
  | maxCapacity < maxFlowPerEdge
  = Just "'maxCapacity' can not be too low for flow weights."
  | maxCapacity < maxTokensPerPlace
  = Just "The starting tokens can not exceed 'maxCapacity'."
  | atLeastActive == 0
  = Just "At least one transition has to be activated."
  | uncurry (>) newArrowsWithComplement
  = Just "The first element of 'newArrowsWithComplement' can not be higher than the second element."
  | fst newArrowsWithComplement < places
  = Just "At least one flow has to be connected to each complement place."
  | snd newArrowsWithComplement > 2 * transitions * places
  = Just "'newArrowsWithComplement' is set unreasonably high, given the number of transitions and places."
  | oneMinCapacity <= 0
  = Just "'oneMinCapacity' has to be positive."
  | oneMinCapacity > maxCapacity
  = Just "'oneMinCapacity' can not be higher than 'maxCapacity'."
  | uncurry (>) distractors
  = Just "The first element of 'distractors' can not be higher than the second element."
  | fst distractors < 0
  = Just "The first element of 'distractors' can not be negative."
  | snd distractors > transitions - atLeastActive
  = Just "'distractors' can not be higher than the number of transitions."
  | otherwise
  = Nothing

defaultCapacityInstance :: CapacityInstance SimplePetriNet
defaultCapacityInstance = CapacityInstance {
  drawWith = DrawSettings {
    withPlaceNames = True,
    withSvgHighlighting = True,
    withTransitionNames = True,
    with1Weights = False,
    withGraphvizCommand = Circo
    },
  toFind = ActivatedTransitions [Reach.Transition 1, Reach.Transition 2],
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
