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
  checkCapacityConfig,
  checkCapacityConfigs,
  combinedCapacity,
  combinedCapacityInstance,
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
import qualified Data.Bimap                       as BM (
  lookup,
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
  renderWith,
  renderWithCapacity,
  )
import Modelling.PetriNet.Find (
  prohibitHidePlaceNames,
  prohibitHideTransitionNames,
  prohibitPatchworkRenderer,
  toFindEvaluationTupleList,
  )
import Modelling.PetriNet.FindActivatedTransitions (
  checkActivatedTransitionsConfig,
  )
import Modelling.PetriNet.Parser (
  addCapacities,
  parseChange,
  parseGivenNet,
  parseRenamedNet,
  simpleNameMap,
  )
import Modelling.PetriNet.Reach.Type (
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types         (
  AdvConfig (..),
  AlloyConfig (..),
  BasicConfig (..),
  Capacity (..),
  CapacityConfig (..),
  DrawSettings (..),
  GraphConfig (..),
  Net (traverseNet),
  PetriChangeList (..),
  PetriLike (PetriLike, allNodes),
  SimpleNode (..),
  SimplePetriNet,
  CapacityNode (..),
  basicConfigBitWidthInput,
  checkBasicConfig,
  petriScopeBitWidth,
  toChangeList,
  )

import Control.Applicative              ((<|>), liftA2)
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
import Data.Foldable                    (for_)
import Data.GraphViz.Commands           (GraphvizCommand (Circo))
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (i, iii)
import Text.Parsec (
  char,
  optionMaybe,
  sepBy,
  spaces,
  )
import Text.Parsec.Char                 (digit)
import Text.Parsec.Combinator           (many1)
import Text.Parsec.String               (Parser)
import Text.Read                        (readMaybe)


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

    (original, transformed, condition) <- combinedCapacityInstance config segment

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

combinedCapacityInstance
  :: (MonadThrow m, RandomGen g, MonadAlloy m, Net p n)
  => CapacityConfig
  -> Int
  -> RandT g m (PetriLike CapacityNode String, p n String, PetriChangeList String)
combinedCapacityInstance = combinedCapacity
  petriNetFindCapacity
  Find.alloyConfig

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
    $=<< renderWithCapacity path "capacityTask" (originalNet task) (drawWith task)
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
        ts = ([("s1",2), ("s2",0)], [("t1","s1",1), ("t2","s1",1), ("s2","t2",2)])
    code $ show ts
    translate $ do
      english ("as answer would indicate that there are two complement places - s1 with 2 tokens and s2 with 0 tokens - and t1 points to s1 with a weight of 1, " ++
               "t2 points to s1 with a weight of 1 and s2 connects to t2 with a weight of 2.")
      german ("als Antwort würde bedeuten, dass es zwei Komplementstellen gibt - s1 mit 2 Token und s2 mit 0 Token - und t1 zeigt auf s1 mit einem Gewicht von 1, " ++
             "t2 zeigt auf s1 mit einem Gewicht von 1, und s2 ist mit t2 mit einem Gewicht von 2 verbunden.")
    translate $ do
      english "The first complement place s1 belongs to the first shown place s3, the second complement place s2 belongs to the second shown place s4 and so on."
      german ("Die erste Komplementstelle s1 gehört zu der ersten gezeigten Stelle s3," ++
             "die zweite Komplementstelle s2 gehört zu der zweiten gezeigten Stelle s4 und so weiter.")
    translate $ do
      english "The order of tuples within the lists does not matter here."
      german "Die Reihenfolge der Tupel innerhalb der Listen spielt hierbei keine Rolle."
    pure ()
  paragraph hoveringInformation
  pure ()

capacitySyntax
  :: OutputCapable m
  => CapacityInstance net
  -> ([(String, Int)], [(String, String, Int)])
  -> LangM' m ()
capacitySyntax task (tokenChanges, flowChanges) = do
  for_ tokenChanges assertTokenChanges
  for_ flowChanges  assertFlowChanges
  pure ()
  where
    assert = continueOrAbort True

    assertTokenChanges (p, tokens) = assert (isValidComplementPlace p && tokens >= 0) $ translate $ do
      let p' = show (p, tokens)
      english $ p' ++ " has a structurally correct form of a complement place?"
      german $ p' ++ " hat eine strukturell korrekte Form einer Komplementstelle?"

    assertFlowChanges (src, tgt, weight) = assert (((isValidComplementPlace src && isValidTransition tgt) ||
                                           (isValidTransition src && isValidComplementPlace tgt)) && weight > 0) $ translate $ do
      let t' = show (src, tgt, weight)
      english $ t' ++ " has a structurally correct form of a flow?"
      german $ t' ++ " hat eine strukturell korrekte Form eines Flusses?"

    isValidComplementPlace :: String -> Bool
    isValidComplementPlace s = case s of
      ('s':rest) -> maybe False (\x -> x >= 1 && x <= (numberOfPlaces task `div` 2)) (readMaybe rest)
      _          -> False

    isValidTransition :: String -> Bool
    isValidTransition t = case t of
      ('t':rest) -> maybe False (\x -> x >= 1 && x <= numberOfTransitions task) (readMaybe rest)
      _          -> False

capacityEvaluation
  :: (Monad m, OutputCapable m)
  => CapacityInstance net
  -> ([(String, Int)], [(String, String, Int)])
  -> Rated m
capacityEvaluation task (tokenChanges, flowChanges) = do
  let whatTokens = translations $ do
        english "The given tuples are added complement places?"
        german "Die angegebenen Tupel sind hinzugefügte Komplementstellen?"
  let whatFlows = translations $ do
        english "The given tuples are added flows?"
        german "Die angegebenen Tupel sind hinzugefügte Flüsse?"
  uncurry (printSolutionAndAssert DefiniteArticle)
    $=<< unLangM $ liftA2 combineResults
      (toFindEvaluationTupleList whatTokens withSol tokens tokenChanges)
      (toFindEvaluationTupleList whatFlows withSol flows flowChanges)

  where
    (tokens, flows) = capacitySolution task
    withSol = showSolution task
    combineResults (list1, points1) (list2, points2) = (combineLists list1 list2, points1 * points2)
    combineLists :: Maybe String -> Maybe String -> Maybe String
    combineLists = liftA2 (\string1 string2 -> string1 ++ "," ++ string2)

capacitySolution :: CapacityInstance net -> ([(String, Int)], [(String, String, Int)])
capacitySolution task = (tokenChanges $ toFind task, flowChanges $ toFind task)

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

  plFirst <- parseGivenNet "defaultFlow" "defaultTokens" inst >>= addCapacities inst
  let nameMapFirst = simpleNameMap plFirst
  first <- traverseNet (`BM.lookup` nameMapFirst) plFirst

  (second, nameMapSecond) <- parseRenamedNet "flow" "tokens" inst
  change <- parseChange inst
  third <- traverse (`BM.lookup` nameMapSecond) (toChangeList change)

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

    parseNodeC :: Parser String
    parseNodeC = do
      tag <- optionMaybe (char 's')
      case tag of
        Just _  -> do
          Reach.Place n <- parsePlacePrec 0
          return $ show n
        Nothing -> do
          Reach.Transition n <- parseTransitionPrec 0
          return $ show n

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
        Just n  -> "  #" ++ activated ++ " <= " ++ show n ++ "\n")
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

skolemName :: String
skolemName = "activatedTrans"

checkCapacityConfigs :: CapacityConfig -> Maybe String
checkCapacityConfigs CapacityConfig {
  basicConfig,
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
  <|> checkCapacityConfig basicConfig maxCapacity newArrowsWithComplement oneMinCapacity distractors atMostActive
  <|> checkActivatedTransitionsConfig basicConfig atMostActive

checkCapacityConfig :: BasicConfig -> Int -> (Int, Int) -> Int -> (Int, Int) -> Maybe Int -> Maybe String
checkCapacityConfig BasicConfig {
    places,
    transitions,
    atLeastActive,
    maxTokensPerPlace,
    maxFlowPerEdge,
    isConnected
    }
  maxCapacity
  newArrowsWithComplement
  oneMinCapacity
  distractors
  atMostActive
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
  | fst distractors > transitions - fromMaybe transitions atMostActive
  = Just "'fst distractors' is set unreasonably high, given atMostActive."
  | snd distractors > transitions - atLeastActive
  = Just "'snd distractors' is set unreasonably high, given atLeastActive."
  | isConnected /= Just True
  = Just "The Petri net must be connected."
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
toFind = ChangeList {
  tokenChanges = [("s1", 1), ("s2", 0)]
  , flowChanges = [("s1", "t2", 1), ("t1", "s1", 1), ("t2", "s2", 1), ("s2", "t1", 1)]
  },
  originalNet = PetriLike {
    allNodes = M.fromList [
      ("s1",CapacityPlace {initial = 0, capacity = 0, flowOut = M.empty}),
      ("s2",CapacityPlace {initial = 0, capacity = 0, flowOut = M.empty}),
      ("s3",CapacityPlace {initial = 1, capacity = 2, flowOut = M.fromList [("t1",1)]}),
      ("s4",CapacityPlace {initial = 0, capacity = 1, flowOut = M.fromList [("t2",1),("t3",1)]}),
      ("t1",CapacityTransition {flowOut = M.fromList [("s4",1)]}),
      ("t2",CapacityTransition {flowOut = M.fromList [("s3",1)]}),
      ("t3",CapacityTransition {flowOut = M.empty})
      ]
    },
  transformedNet = PetriLike {
    allNodes = M.fromList [
      ("s1",SimplePlace {initial = 1, flowOut = M.fromList [("t2",1)]}),
      ("s2",SimplePlace {initial = 0, flowOut = M.fromList [("s1",1)]}),
      ("s3",SimplePlace {initial = 1, flowOut = M.fromList [("t1",1)]}),
      ("s4",SimplePlace {initial = 0, flowOut = M.fromList [("t2",1),("t3",1)]}),
      ("t1",SimpleTransition {flowOut = M.fromList [("s4",1),("s1",1)]}),
      ("t2",SimpleTransition {flowOut = M.fromList [("s3",1),("s2",1)]}),
      ("t3",SimpleTransition {flowOut = M.fromList [("s2",1)]})
      ]
    },
  numberOfPlaces = 4,
  numberOfTransitions = 3,
  showSolution = False
}
