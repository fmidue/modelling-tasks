{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wwarn=deprecations #-}

module Modelling.ActivityDiagram.SelectPetri (
  SelectPetriInstance(..),
  SelectPetriConfig(..),
  SelectPetriSolution(..),
  defaultSelectPetriConfig,
  checkSelectPetriConfig,
  checkPetriInstance,
  selectPetriAlloy,
  selectPetrinet,
  selectPetriTask,
  selectPetriSyntax,
  selectPetriEvaluation,
  selectPetriSolution,
  selectPetri,
  defaultSelectPetriInstance
  ) where

import qualified Data.Map as M (empty, size, fromList, toList, keys, map, filter)
import qualified Modelling.ActivityDiagram.Datatype as AD (ADNode(label))
import qualified Modelling.ActivityDiagram.Petrinet as PK (PetriKey(label))

import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..), isInitialNode, isActivityFinalNode, isFlowFinalNode)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.Isomorphism (isPetriIsomorphic)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.PlantUMLConverter (drawADToFile, defaultPlantUMLConvConf)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames, shufflePetri)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith, headWithErr)

import Modelling.Auxiliary.Common (oneOf)
import Modelling.Auxiliary.Output (addPretext)
import Modelling.PetriNet.Diagram (cacheNet)
import Modelling.PetriNet.Types (PetriLike(..), Node(..))

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  LangM,
  Rated,
  OutputMonad (..),
  english,
  german,
  translate,
  translations,
  singleChoice, singleChoiceSyntax
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.Except (runExceptT)
import Data.List (unfoldr, nubBy, sortOn)
import Data.List.Extra (dropEnd)
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Graph.Inductive (Gr, mkGraph, lab, level)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.String.Interpolate ( i )
import Language.Alloy.Call (getInstances)
import System.Random (next)       --To be changed from 'next' to 'uniform', not possible as of now due to dependencies
import System.Random.Shuffle (shuffle', shuffleM)


data SelectPetriInstance = SelectPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  graphvizCmd :: GraphvizCommand,
  petrinets :: Map Int (Bool, PetriLike PetriKey)
} deriving (Show)

data SelectPetriConfig = SelectPetriConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  petriLayout :: [GraphvizCommand],
  numberOfWrongAnswers :: Int,
  numberOfModifications :: Int,
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Show)

pickRandomLayout :: (MonadRandom m) => SelectPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultSelectPetriConfig :: SelectPetriConfig
defaultSelectPetriConfig = SelectPetriConfig {
  adConfig = defaultADConfig,
  maxInstances = Just 50,
  petriLayout = [Dot],
  numberOfWrongAnswers = 2,
  numberOfModifications = 3,
  supportSTAbsent = Nothing,
  activityFinalsExist = Just True,
  avoidAddingSinksForFinals = Nothing,
  noActivityFinalInForkBlocks = Just False
}

checkSelectPetriConfig :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectPetriConfig' conf

checkSelectPetriConfig' :: SelectPetriConfig -> Maybe String
checkSelectPetriConfig' SelectPetriConfig {
    adConfig,
    maxInstances,
    petriLayout,
    numberOfWrongAnswers,
    numberOfModifications,
    supportSTAbsent,
    activityFinalsExist,
    avoidAddingSinksForFinals,
    noActivityFinalInForkBlocks
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
  | numberOfWrongAnswers < 1
    = Just "The parameter 'numberOfWrongAnswers' must be set to a positive value"
  | numberOfModifications < 1
    = Just "The parameter 'numberOfModifications' must be set to a positive value"
  | supportSTAbsent == Just True && cycles adConfig > 0
    = Just "Setting the parameter 'supportSTAbsent' to True prohibits having more than 0 cycles"
  | activityFinalsExist == Just True && activityFinalNodes adConfig < 1
    = Just "Setting the parameter 'activityFinalsExist' to True implies having at least 1 Activity Final Node"
  | activityFinalsExist == Just False && activityFinalNodes adConfig > 0
    = Just "Setting the parameter 'activityFinalsExist' to False prohibits having more than 0 Activity Final Node"
  | avoidAddingSinksForFinals == Just True && minActions adConfig + forkJoinPairs adConfig < 1
    = Just "The option 'avoidAddingSinksForFinals' can only be achieved if the number of Actions, Fork Nodes and Join Nodes together is positive"
  | noActivityFinalInForkBlocks == Just True && activityFinalNodes adConfig > 1
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to True prohibits having more than 1 Activity Final Node"
  | noActivityFinalInForkBlocks == Just False && activityFinalsExist /= Just True
    = Just "Setting the parameter 'noActivityFinalInForkBlocks' to False implies that the parameter 'activityFinalsExit' should be True"
  | null petriLayout
    = Just "The parameter 'petriLayout' can not be the empty list"
  | any (`notElem` [Dot, Neato, TwoPi, Circo, Fdp]) petriLayout
    = Just "The parameter 'petriLayout' can only contain the options Dot, Neato, TwoPi, Circo and Fdp"
  | otherwise
    = Nothing

selectPetriAlloy :: SelectPetriConfig -> String
selectPetriAlloy SelectPetriConfig {
  adConfig,
  supportSTAbsent,
  activityFinalsExist,
  avoidAddingSinksForFinals,
  noActivityFinalInForkBlocks
}
  = adConfigToAlloy modules preds adConfig
  where modules = modulePetrinet
        preds =
          [i|
            #{f supportSTAbsent "supportSTAbsent"}
            #{f activityFinalsExist "activityFinalsExist"}
            #{f avoidAddingSinksForFinals "avoidAddingSinksForFinals"}
            #{f noActivityFinalInForkBlocks "noActivityFinalInForkBlocks"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

checkPetriInstance :: SelectPetriInstance -> SelectPetriConfig -> Maybe String
checkPetriInstance inst SelectPetriConfig {
    numberOfWrongAnswers
  }
  | M.size (M.filter (not . fst) $ petrinets inst) /= numberOfWrongAnswers
    = Just "Number of wrong nets found for given instance is unequal to numberOfWrongAnswers"
  | otherwise
    = Nothing

data SelectPetriSolution = SelectPetriSolution {
  matchingNet :: PetriLike PetriKey,
  wrongNets :: [PetriLike PetriKey]
} deriving (Show)

selectPetrinet :: Int -> Int -> Int -> UMLActivityDiagram -> SelectPetriSolution
selectPetrinet numberOfWrongNets numberOfModifications seed ad =
  let matchingNet = convertToPetrinet ad
      seeds = unfoldr (Just . next) (mkStdGen seed)
      wrongNets = take numberOfWrongNets
                  $ nubBy isPetriIsomorphic
                  $ filter (not . isPetriIsomorphic matchingNet)
                  $ map (convertToPetrinet . modifyAD ad False numberOfModifications) seeds
  in SelectPetriSolution {matchingNet=matchingNet, wrongNets=wrongNets}

modifyAD :: UMLActivityDiagram -> Bool -> Int -> Int -> UMLActivityDiagram
modifyAD diag pickFromMid numberOfModifications seed =
  let sampler = if pickFromMid then pickRandomItemsFromMid else pickRandomItems
      ns = if pickFromMid then map fst $ sortOn snd $ distToStartNode diag else nodes diag
      filteredNodes = filter (\x ->
        not (isInitialNode x) &&
        not (isActivityFinalNode x) &&
        not (isFlowFinalNode x)) ns
      toBeModified = sampler numberOfModifications filteredNodes seed
      swappedNodes = map (\x -> if x `elem` toBeModified then swapST x else x) $ nodes diag
  in UMLActivityDiagram {nodes=swappedNodes, connections=connections diag}

-- Swap nodes translated to places to nodes translated to transitions and vice versa
swapST :: ADNode -> ADNode
swapST node =
  case node of
    ADActionNode {label, name} -> ADObjectNode {label=label, name=name}
    ADObjectNode {label, name} -> ADActionNode {label=label, name=name}
    ADDecisionNode {label} -> ADForkNode {label}
    ADForkNode {label} -> ADDecisionNode {label}
    ADMergeNode {label} -> ADJoinNode {label}
    ADJoinNode {label} -> ADMergeNode {label}
    _ -> node

pickRandomItems :: Int -> [a] -> Int -> [a]
pickRandomItems n xs seed =
  take n $ shuffle' xs (length xs) (mkStdGen seed)

distToStartNode :: UMLActivityDiagram -> [(ADNode, Int)]
distToStartNode diag =
  let startNode = head $ map AD.label $ filter isInitialNode $ nodes diag
      grNodes = map (\x -> (AD.label x, x)) $ nodes diag
      grEdges = map (\x -> (from x, to x, guard x)) $ connections diag
      graph = mkGraph grNodes grEdges :: Gr ADNode String
  in map (\(x,y) -> (fromJust $ lab graph x, y)) $ level startNode graph

pickRandomItemsFromMid :: Int -> [a] -> Int -> [a]
pickRandomItemsFromMid n xs seed =
  let m = length xs `div` 3
      mid = dropEnd m $ drop m xs
  in pickRandomItems n mid seed

selectPetriTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectPetriInstance
  -> LangM m
selectPetriTask path task = do
  let mapping = M.map snd $ petrinets task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  petris <- liftIO $
    traverse (\c -> runExceptT
      $ cacheNet path (show . PK.label) c False False True (graphvizCmd task)) mapping
  paragraph $ translate $ do
    english "Consider the following petrinets."
    german "Betrachten Sie die folgenden Petrinetze."
  images show id (M.map (failWith id) petris)
  paragraph $ translate $ do
    english [i|Which of these petrinets matches the given activity diagram?
Please state your answer by giving a number indicating the matching petrinet.|]
    german [i|Welcher dieser Petrinetze passt zum gegebenen Aktivitätsdiagramm?
Bitte geben Sie ihre Antwort als Zahl an, welche das passende Petrinetz repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "2"
    translate $ do
      english [i|would indicate that petrinet 2 is the matching petrinet.|]
      german  [i|würde bedeuten, dass Petrinetz 2 das passende Petrinetz ist.|]

selectPetriSolutionToMap
  :: Int
  -> SelectPetriSolution
  -> Map Int (Bool, PetriLike PetriKey)
selectPetriSolutionToMap seed sol =
  let xs = (True, matchingNet sol) : map (False, ) (wrongNets sol)
      solution = shuffle' xs (length xs) (mkStdGen seed)
  in M.fromList $ zip [1..] solution

selectPetriSyntax
  :: (OutputMonad m)
  => SelectPetriInstance
  -> Int
  -> LangM m
selectPetriSyntax task sub = addPretext $ do
  let options = M.keys $ petrinets task
  singleChoiceSyntax True options sub

selectPetriEvaluation
  :: OutputMonad m
  => SelectPetriInstance
  -> Int
  -> Rated m
selectPetriEvaluation task n = addPretext $ do
  let as = translations $ do
        english "petrinet"
        german "Petrinet"
      solMap = petrinets task
      (solution, _) = head $ M.toList $ M.map snd $ M.filter fst solMap
  singleChoice as (Just $ show solution) solution n

selectPetriSolution
  :: SelectPetriInstance
  -> Int
selectPetriSolution = head . M.keys . M.filter fst . petrinets

selectPetri
  :: SelectPetriConfig
  -> Int
  -> Int
  -> IO SelectPetriInstance
selectPetri config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectPetriTask config) g

getSelectPetriTask
  :: (RandomGen g, MonadIO m)
  => SelectPetriConfig
  -> RandT g m SelectPetriInstance
getSelectPetriTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ selectPetriAlloy config
  rinstas <- shuffleM instas
  n <- getRandom
  g' <- getRandom
  layout <- pickRandomLayout config
  let ad = map (snd . shuffleADNames n . failWith id . parseInstance) rinstas
      validInsta =
        headWithErr "Failed to find task instances"
        $ filter (isNothing . (`checkPetriInstance` config))
        $ map (\x ->
          SelectPetriInstance {
            activityDiagram=x,
            seed=g',
            graphvizCmd=layout,
            petrinets= selectPetriSolutionToMap g'
              $ shuffleSolutionNets n
              $ selectPetrinet (numberOfWrongAnswers config) (numberOfModifications config) n x
          }) ad
  return validInsta

shuffleSolutionNets :: Int -> SelectPetriSolution -> SelectPetriSolution
shuffleSolutionNets n sol = SelectPetriSolution {
  matchingNet = snd $ shufflePetri n (matchingNet sol),
  wrongNets =  map (snd . shufflePetri n) (wrongNets sol)
}

defaultSelectPetriInstance :: SelectPetriInstance
defaultSelectPetriInstance =  SelectPetriInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      ADActionNode {label = 1, name = "A"},
      ADActionNode {label = 2, name = "B"},
      ADActionNode {label = 3, name = "E"},
      ADActionNode {label = 4, name = "G"},
      ADObjectNode {label = 5, name = "D"},
      ADObjectNode {label = 6, name = "C"},
      ADObjectNode {label = 7, name = "F"},
      ADObjectNode {label = 8, name = "H"},
      ADDecisionNode {label = 9},
      ADDecisionNode {label = 10},
      ADMergeNode {label = 11},
      ADMergeNode {label = 12},
      ADForkNode {label = 13},
      ADJoinNode {label = 14},
      ADActivityFinalNode {label = 15},
      ADFlowFinalNode {label = 16},
      ADInitialNode {label = 17}
    ],
    connections = [
      ADConnection {from = 1, to = 12, guard = ""},
      ADConnection {from = 2, to = 12, guard = ""},
      ADConnection {from = 3, to = 10, guard = ""},
      ADConnection {from = 4, to = 15, guard = ""},
      ADConnection {from = 5, to = 14, guard = ""},
      ADConnection {from = 6, to = 14, guard = ""},
      ADConnection {from = 7, to = 13, guard = ""},
      ADConnection {from = 8, to = 4, guard = ""},
      ADConnection {from = 9, to = 1, guard = "b"},
      ADConnection {from = 9, to = 2, guard = "a"},
      ADConnection {from = 10, to = 9, guard = "b"},
      ADConnection {from = 10, to = 11, guard = "a"},
      ADConnection {from = 11, to = 3, guard = ""},
      ADConnection {from = 12, to = 8, guard = ""},
      ADConnection {from = 13, to = 5, guard = ""},
      ADConnection {from = 13, to = 6, guard = ""},
      ADConnection {from = 13, to = 11, guard = ""},
      ADConnection {from = 14, to = 16, guard = ""},
      ADConnection {from = 17, to = 7, guard = ""}
    ]
  },
  seed = -4748947987859297750,
  graphvizCmd = Dot,
  petrinets = M.fromList [
    (1,(False, PetriLike {
      allNodes = M.fromList [
        (NormalST {label = 1, sourceNode = ADActionNode {label = 2, name = "B"}},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 20, sourceNode = ADDecisionNode {label = 9}},1)],
          flowOut = M.fromList [(SupportST {label = 9},1)]}),
        (SupportST {label = 2},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 17, sourceNode = ADActionNode {label = 1, name = "A"}},1)],
          flowOut = M.fromList [(NormalST {label = 24, sourceNode = ADJoinNode {label = 12}},1)]}),
        (NormalST {label = 3, sourceNode = ADActionNode {label = 8, name = "H"}},
        TransitionNode {
          flowIn = M.fromList [(SupportST {label = 10},1)],
          flowOut = M.fromList [(SupportST {label = 18},1)]}),
        (NormalST {label = 4, sourceNode = ADDecisionNode {label = 10}},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 8, sourceNode = ADActionNode {label = 3, name = "E"}},1)],
          flowOut = M.fromList [(SupportST {label = 6},1),(SupportST {label = 23},1)]}),
        (SupportST {label = 5},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 21, sourceNode = ADForkNode {label = 13}},1)],
          flowOut = M.fromList [(NormalST {label = 7, sourceNode = ADActionNode {label = 6, name = "C"}},1)]}),
        (SupportST {label = 6},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 4, sourceNode = ADDecisionNode {label = 10}},1)],
          flowOut = M.fromList [(NormalST {label = 20, sourceNode = ADDecisionNode {label = 9}},1)]}),
        (NormalST {label = 7, sourceNode = ADActionNode {label = 6, name = "C"}},
        TransitionNode {
          flowIn = M.fromList [(SupportST {label = 5},1)],
          flowOut = M.fromList [(SupportST {label = 12},1)]}),
        (NormalST {label = 8, sourceNode = ADActionNode {label = 3, name = "E"}},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 11, sourceNode = ADMergeNode {label = 11}},1)],
          flowOut = M.fromList [(NormalST {label = 4, sourceNode = ADDecisionNode {label = 10}},1)]}),
        (SupportST {label = 9},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 1, sourceNode = ADActionNode {label = 2, name = "B"}},1)],
          flowOut = M.fromList [(NormalST {label = 24, sourceNode = ADJoinNode {label = 12}},1)]}),
        (SupportST {label = 10},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 24, sourceNode = ADJoinNode {label = 12}},1)],
          flowOut = M.fromList [(NormalST {label = 3, sourceNode = ADActionNode {label = 8, name = "H"}},1)]}),
        (NormalST {label = 11, sourceNode = ADMergeNode {label = 11}},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 21, sourceNode = ADForkNode {label = 13}},1),(SupportST {label = 23},1)],
          flowOut = M.fromList [(NormalST {label = 8, sourceNode = ADActionNode {label = 3, name = "E"}},1)]}),
        (SupportST {label = 12},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 7, sourceNode = ADActionNode {label = 6, name = "C"}},1)],
          flowOut = M.fromList [(NormalST {label = 14, sourceNode = ADJoinNode {label = 14}},1)]}),
        (NormalST {label = 13, sourceNode = ADActionNode {label = 4, name = "G"}},
        TransitionNode {
          flowIn = M.fromList [(SupportST {label = 18},1)],
          flowOut = M.empty}),
        (NormalST {label = 14, sourceNode = ADJoinNode {label = 14}},
        TransitionNode {
          flowIn = M.fromList [(SupportST {label = 12},1),(NormalST {label = 22, sourceNode = ADObjectNode {label = 5, name = "D"}},1)],
          flowOut = M.empty}),
        (NormalST {label = 15, sourceNode = ADObjectNode {label = 7, name = "F"}},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(SupportST {label = 19},1)],
          flowOut = M.fromList [(NormalST {label = 21, sourceNode = ADForkNode {label = 13}},1)]}),
        (NormalST {label = 16, sourceNode = ADInitialNode {label = 17}},
        PlaceNode {
          initial = 1,
          flowIn = M.empty,
          flowOut = M.fromList [(SupportST {label = 19},1)]}),
        (NormalST {label = 17, sourceNode = ADActionNode {label = 1, name = "A"}},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 20, sourceNode = ADDecisionNode {label = 9}},1)],
          flowOut = M.fromList [(SupportST {label = 2},1)]}),
        (SupportST {label = 18},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 3, sourceNode = ADActionNode {label = 8, name = "H"}},1)],
          flowOut = M.fromList [(NormalST {label = 13, sourceNode = ADActionNode {label = 4, name = "G"}},1)]}),
        (SupportST {label = 19},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 16, sourceNode = ADInitialNode {label = 17}},1)],
          flowOut = M.fromList [(NormalST {label = 15, sourceNode = ADObjectNode {label = 7, name = "F"}},1)]}),
        (NormalST {label = 20, sourceNode = ADDecisionNode {label = 9}},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(SupportST {label = 6},1)],
          flowOut = M.fromList [(NormalST {label = 1, sourceNode = ADActionNode {label = 2, name = "B"}},1),
            (NormalST {label = 17, sourceNode = ADActionNode {label = 1, name = "A"}},1)]}),
        (NormalST {label = 21, sourceNode = ADForkNode {label = 13}},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 15, sourceNode = ADObjectNode {label = 7, name = "F"}},1)],
          flowOut = M.fromList [(SupportST {label = 5},1),
            (NormalST {label = 11, sourceNode = ADMergeNode {label = 11}},1),
            (NormalST {label = 22, sourceNode = ADObjectNode {label = 5, name = "D"}},1)]}),
        (NormalST {label = 22, sourceNode = ADObjectNode {label = 5, name = "D"}},
        PlaceNode {
          initial = 0,
          flowIn = M.fromList [(NormalST {label = 21, sourceNode = ADForkNode {label = 13}},1)],
          flowOut = M.fromList [(NormalST {label = 14, sourceNode = ADJoinNode {label = 14}},1)]}),
        (SupportST {label = 23},
        TransitionNode {
          flowIn = M.fromList [(NormalST {label = 4, sourceNode = ADDecisionNode {label = 10}},1)],
          flowOut = M.fromList [(NormalST {label = 11, sourceNode = ADMergeNode {label = 11}},1)]}),
        (NormalST {label = 24, sourceNode = ADJoinNode {label = 12}},
        TransitionNode {
          flowIn = M.fromList [(SupportST {label = 2},1),(SupportST {label = 9},1)],
          flowOut = M.fromList [(SupportST {label = 10},1)]})
      ]
    }
  )),
  (2,(True,PetriLike {
    allNodes = M.fromList [
      (NormalST {label = 1, sourceNode = ADForkNode {label = 13}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 6, sourceNode = ADObjectNode {label = 7, name = "F"}},1)],
        flowOut = M.fromList [(NormalST {label = 3, sourceNode = ADObjectNode {label = 5, name = "D"}},1),
          (NormalST {label = 5, sourceNode = ADObjectNode {label = 6, name = "C"}},1),
          (NormalST {label = 12, sourceNode = ADMergeNode {label = 11}},1)]}),
      (SupportST {label = 2},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 10, sourceNode = ADInitialNode {label = 17}},1)],
        flowOut = M.fromList [(NormalST {label = 6, sourceNode = ADObjectNode {label = 7, name = "F"}},1)]}),
      (NormalST {label = 3, sourceNode = ADObjectNode {label = 5, name = "D"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 1, sourceNode = ADForkNode {label = 13}},1)],
        flowOut = M.fromList [(NormalST {label = 19, sourceNode = ADJoinNode {label = 14}},1)]}),
      (NormalST {label = 4, sourceNode = ADActionNode {label = 4, name = "G"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 16, sourceNode = ADObjectNode {label = 8, name = "H"}},1)],
        flowOut = M.empty}),
      (NormalST {label = 5, sourceNode = ADObjectNode {label = 6, name = "C"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 1, sourceNode = ADForkNode {label = 13}},1)],
        flowOut = M.fromList [(NormalST {label = 19, sourceNode = ADJoinNode {label = 14}},1)]}),
      (NormalST {label = 6, sourceNode = ADObjectNode {label = 7, name = "F"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 2},1)],
        flowOut = M.fromList [(NormalST {label = 1, sourceNode = ADForkNode {label = 13}},1)]}),
      (NormalST {label = 7, sourceNode = ADDecisionNode {label = 9}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 13},1)],
        flowOut = M.fromList [(NormalST {label = 9, sourceNode = ADActionNode {label = 1, name = "A"}},1),
          (NormalST {label = 11, sourceNode = ADActionNode {label = 2, name = "B"}},1)]}),
      (NormalST {label = 8, sourceNode = ADMergeNode {label = 12}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 9, sourceNode = ADActionNode {label = 1, name = "A"}},1),
          (NormalST {label = 11, sourceNode = ADActionNode {label = 2, name = "B"}},1)],
        flowOut = M.fromList [(SupportST {label = 18},1)]}),
      (NormalST {label = 9, sourceNode = ADActionNode {label = 1, name = "A"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 7, sourceNode = ADDecisionNode {label = 9}},1)],
        flowOut = M.fromList [(NormalST {label = 8, sourceNode = ADMergeNode {label = 12}},1)]}),
      (NormalST {label = 10, sourceNode = ADInitialNode {label = 17}},
      PlaceNode {
        initial = 1,
        flowIn = M.empty,
        flowOut = M.fromList [(SupportST {label = 2},1)]}),
      (NormalST {label = 11, sourceNode = ADActionNode {label = 2, name = "B"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 7, sourceNode = ADDecisionNode {label = 9}},1)],
        flowOut = M.fromList [(NormalST {label = 8, sourceNode = ADMergeNode {label = 12}},1)]}),
      (NormalST {label = 12, sourceNode = ADMergeNode {label = 11}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 1, sourceNode = ADForkNode {label = 13}},1),
          (SupportST {label = 17},1)],
        flowOut = M.fromList [(NormalST {label = 15, sourceNode = ADActionNode {label = 3, name = "E"}},1)]}),
      (SupportST {label = 13},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 14, sourceNode = ADDecisionNode {label = 10}},1)],
        flowOut = M.fromList [(NormalST {label = 7, sourceNode = ADDecisionNode {label = 9}},1)]}),
      (NormalST {label = 14, sourceNode = ADDecisionNode {label = 10}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 15, sourceNode = ADActionNode {label = 3, name = "E"}},1)],
        flowOut = M.fromList [(SupportST {label = 13},1), (SupportST {label = 17},1)]}),
      (NormalST {label = 15, sourceNode = ADActionNode {label = 3, name = "E"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 12, sourceNode = ADMergeNode {label = 11}},1)],
        flowOut = M.fromList [(NormalST {label = 14, sourceNode = ADDecisionNode {label = 10}},1)]}),
      (NormalST {label = 16, sourceNode = ADObjectNode {label = 8, name = "H"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 18},1)],
        flowOut = M.fromList [(NormalST {label = 4, sourceNode = ADActionNode {label = 4, name = "G"}},1)]}),
      (SupportST {label = 17},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 14, sourceNode = ADDecisionNode {label = 10}},1)],
        flowOut = M.fromList [(NormalST {label = 12, sourceNode = ADMergeNode {label = 11}},1)]}),
      (SupportST {label = 18},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 8, sourceNode = ADMergeNode {label = 12}},1)],
        flowOut = M.fromList [(NormalST {label = 16, sourceNode = ADObjectNode {label = 8, name = "H"}},1)]}),
      (NormalST {label = 19, sourceNode = ADJoinNode {label = 14}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 3, sourceNode = ADObjectNode {label = 5, name = "D"}},1),
          (NormalST {label = 5, sourceNode = ADObjectNode {label = 6, name = "C"}},1)],
        flowOut = M.empty})
    ]
  })),
  (3,(False,PetriLike {
    allNodes = M.fromList [
      (NormalST {label = 1, sourceNode = ADJoinNode {label = 12}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 15, sourceNode = ADObjectNode {label = 2, name = "B"}},1),
          (NormalST {label = 17, sourceNode = ADObjectNode {label = 1, name = "A"}},1)],
        flowOut = M.fromList [(NormalST {label = 20, sourceNode = ADObjectNode {label = 8, name = "H"}},1)]}),
      (SupportST {label = 2},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 12, sourceNode = ADDecisionNode {label = 10}},1)],
        flowOut = M.fromList [(NormalST {label = 6, sourceNode = ADDecisionNode {label = 9}},1)]}),
      (NormalST {label = 3, sourceNode = ADActionNode {label = 3, name = "E"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 19, sourceNode = ADMergeNode {label = 11}},1)],
        flowOut = M.fromList [(NormalST {label = 12, sourceNode = ADDecisionNode {label = 10}},1)]}),
      (SupportST {label = 4},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 14, sourceNode = ADInitialNode {label = 17}},1)],
        flowOut = M.fromList [(NormalST {label = 9, sourceNode = ADObjectNode {label = 7, name = "F"}},1)]}),
      (SupportST {label = 5},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 12, sourceNode = ADDecisionNode {label = 10}},1)],
        flowOut = M.fromList [(NormalST {label = 19, sourceNode = ADMergeNode {label = 11}},1)]}),
      (NormalST {label = 6, sourceNode = ADDecisionNode {label = 9}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 2},1)],
        flowOut = M.fromList [(SupportST {label = 8},1),(SupportST {label = 18},1)]}),
      (NormalST {label = 7, sourceNode = ADObjectNode {label = 5, name = "D"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 16, sourceNode = ADForkNode {label = 13}},1)],
        flowOut = M.fromList [(NormalST {label = 11, sourceNode = ADJoinNode {label = 14}},1)]}),
      (SupportST {label = 8},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 6, sourceNode = ADDecisionNode {label = 9}},1)],
        flowOut = M.fromList [(NormalST {label = 17, sourceNode = ADObjectNode {label = 1, name = "A"}},1)]}),
      (NormalST {label = 9, sourceNode = ADObjectNode {label = 7, name = "F"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 4},1)],
        flowOut = M.fromList [(NormalST {label = 16, sourceNode = ADForkNode {label = 13}},1)]}),
      (NormalST {label = 10, sourceNode = ADActionNode {label = 4, name = "G"}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 20, sourceNode = ADObjectNode {label = 8, name = "H"}},1)],
        flowOut = M.empty}),
      (NormalST {label = 11, sourceNode = ADJoinNode {label = 14}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 7, sourceNode = ADObjectNode {label = 5, name = "D"}},1),
          (NormalST {label = 13, sourceNode = ADObjectNode {label = 6, name = "C"}},1)],
        flowOut = M.empty}),
      (NormalST {label = 12, sourceNode = ADDecisionNode {label = 10}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 3, sourceNode = ADActionNode {label = 3, name = "E"}},1)],
        flowOut = M.fromList [(SupportST {label = 2},1),(SupportST {label = 5},1)]}),
      (NormalST {label = 13, sourceNode = ADObjectNode {label = 6, name = "C"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 16, sourceNode = ADForkNode {label = 13}},1)],
        flowOut = M.fromList [(NormalST {label = 11, sourceNode = ADJoinNode {label = 14}},1)]}),
      (NormalST {label = 14, sourceNode = ADInitialNode {label = 17}},
      PlaceNode {
        initial = 1,
        flowIn = M.empty,
        flowOut = M.fromList [(SupportST {label = 4},1)]}),
      (NormalST {label = 15, sourceNode = ADObjectNode {label = 2, name = "B"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 18},1)],
        flowOut = M.fromList [(NormalST {label = 1, sourceNode = ADJoinNode {label = 12}},1)]}),
      (NormalST {label = 16, sourceNode = ADForkNode {label = 13}},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 9, sourceNode = ADObjectNode {label = 7, name = "F"}},1)],
        flowOut = M.fromList [(NormalST {label = 7, sourceNode = ADObjectNode {label = 5, name = "D"}},1),
          (NormalST {label = 13, sourceNode = ADObjectNode {label = 6, name = "C"}},1),
          (NormalST {label = 19, sourceNode = ADMergeNode {label = 11}},1)]}),
      (NormalST {label = 17, sourceNode = ADObjectNode {label = 1, name = "A"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 8},1)],
        flowOut = M.fromList [(NormalST {label = 1, sourceNode = ADJoinNode {label = 12}},1)]}),
      (SupportST {label = 18},
      TransitionNode {
        flowIn = M.fromList [(NormalST {label = 6, sourceNode = ADDecisionNode {label = 9}},1)],
        flowOut = M.fromList [(NormalST {label = 15, sourceNode = ADObjectNode {label = 2, name = "B"}},1)]}),
      (NormalST {label = 19, sourceNode = ADMergeNode {label = 11}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(SupportST {label = 5},1),(NormalST {label = 16, sourceNode = ADForkNode {label = 13}},1)],
        flowOut = M.fromList [(NormalST {label = 3, sourceNode = ADActionNode {label = 3, name = "E"}},1)]}),
      (NormalST {label = 20, sourceNode = ADObjectNode {label = 8, name = "H"}},
      PlaceNode {
        initial = 0,
        flowIn = M.fromList [(NormalST {label = 1, sourceNode = ADJoinNode {label = 12}},1)],
        flowOut = M.fromList [(NormalST {label = 10, sourceNode = ADActionNode {label = 4, name = "G"}},1)]})
    ]
  }))]
}