{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.MatchPetri (
  MatchPetriInstance(..),
  MatchPetriConfig(..),
  MatchPetriSolution(..),
  defaultMatchPetriConfig,
  checkMatchPetriConfig,
  matchPetriAlloy,
  matchPetriSolution,
  extractSupportSTs,
  matchPetriTask,
  matchPetriInitial,
  matchPetriSyntax,
  matchPetriEvaluation,
  matchPetri,
  defaultMatchPetriInstance
) where

import qualified Data.Map as M (empty, fromList, keys, null)
import qualified Modelling.ActivityDiagram.Petrinet as PK (label)
import qualified Modelling.PetriNet.Types as Petri (Net (nodes))

import Modelling.ActivityDiagram.Datatype (
  UMLActivityDiagram(..),
  ADNode(..),
  ADConnection(..),
  isActionNode,
  isObjectNode,
  isDecisionNode,
  isMergeNode,
  isJoinNode,
  isInitialNode,
  isForkNode
  )
import Modelling.ActivityDiagram.Isomorphism (petriHasMultipleAutomorphisms)
import Modelling.ActivityDiagram.Petrinet (PetriKey(..), convertToPetrinet)
import Modelling.ActivityDiagram.Shuffle (shufflePetri, shuffleADNames)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Alloy (modulePetrinet)
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (PlantUMLConvConf(..), defaultPlantUMLConvConf, drawADToFile)
import Modelling.ActivityDiagram.Auxiliary.Util (failWith, headWithErr)

import Modelling.Auxiliary.Common (oneOf)
import Modelling.Auxiliary.Output (addPretext)
import Modelling.PetriNet.Diagram (cacheNet)
import Modelling.PetriNet.Types (
  DrawSettings (..),
  Net (outFlow),
  PetriLike (..),
  SimpleNode (..),
  SimplePetriLike,
  )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Rated,
  OutputMonad,
  ($=<<),
  english,
  german,
  translate,
  translations,
  multipleChoice,
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen
  )
import Data.Bifunctor                   (second)
import Data.GraphViz.Commands (GraphvizCommand(..))
import Data.List (sort)
import Data.Map (Map)
import Data.Maybe (isJust, fromJust)
import Data.String.Interpolate (i, iii)
import Data.Tuple.Extra                 (dupe)
import GHC.Generics (Generic)
import Language.Alloy.Call (getInstances)
import System.Random.Shuffle (shuffleM)


data MatchPetriInstance = MatchPetriInstance {
  activityDiagram :: UMLActivityDiagram,
  petrinet :: SimplePetriLike PetriKey,
  plantUMLConf :: PlantUMLConvConf,
  petriDrawConf :: DrawSettings
} deriving (Generic, Show)

data MatchPetriConfig = MatchPetriConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  hideBranchConditions :: Bool,
  petriLayout :: [GraphvizCommand],
  supportSTAbsent :: Maybe Bool,            -- Option to prevent support STs from occurring
  activityFinalsExist :: Maybe Bool,        -- Option to disallow activity finals to reduce semantic confusion
  avoidAddingSinksForFinals :: Maybe Bool,  -- Avoid having to add new sink transitions for representing finals
  noActivityFinalInForkBlocks :: Maybe Bool -- Avoid Activity Finals in concurrent flows to reduce confusion
} deriving (Generic, Show)

pickRandomLayout :: (MonadRandom m) => MatchPetriConfig -> m GraphvizCommand
pickRandomLayout conf = oneOf (petriLayout conf)

defaultMatchPetriConfig :: MatchPetriConfig
defaultMatchPetriConfig = MatchPetriConfig
  { adConfig = defaultADConfig,
    maxInstances = Just 50,
    hideBranchConditions = False,
    petriLayout = [Dot],
    supportSTAbsent = Nothing,
    activityFinalsExist = Just True,
    avoidAddingSinksForFinals = Nothing,
    noActivityFinalInForkBlocks = Just False
  }

checkMatchPetriConfig :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig conf =
  checkADConfig (adConfig conf)
  <|> checkMatchPetriConfig' conf


checkMatchPetriConfig' :: MatchPetriConfig -> Maybe String
checkMatchPetriConfig' MatchPetriConfig {
    adConfig,
    maxInstances,
    petriLayout,
    supportSTAbsent,
    activityFinalsExist,
    avoidAddingSinksForFinals,
    noActivityFinalInForkBlocks
  }
  | isJust maxInstances && fromJust maxInstances < 1
    = Just "The parameter 'maxInstances' must either be set to a postive value or to Nothing"
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


matchPetriAlloy :: MatchPetriConfig -> String
matchPetriAlloy MatchPetriConfig {
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

mapTypesToLabels
  :: Net p n
  => p n PetriKey
  -> MatchPetriSolution
mapTypesToLabels petri =
  MatchPetriSolution {
    actionNodes = extractNameLabelTuple isActionNode,
    objectNodes = extractNameLabelTuple isObjectNode,
    decisionNodes = extractLabels isDecisionNode,
    mergeNodes = extractLabels isMergeNode,
    forkNodes = extractLabels isForkNode,
    joinNodes = extractLabels isJoinNode,
    initialNodes = extractLabels isInitialNode,
    supportSTs = sort $ map PK.label $ extractSupportSTs petri
  }
  where
    extractNameLabelTuple fn =
      sort $
      map (\k -> (name $ sourceNode k, PK.label k)) $
      keysByNodeType fn
    extractLabels fn =
      sort $
      map PK.label $
      keysByNodeType fn
    keysByNodeType fn =
      filter (fn . sourceNode)  $
      filter (not . isSupportST) $
      M.keys $ Petri.nodes petri

data MatchPetriSolution = MatchPetriSolution {
  actionNodes :: [(String, Int)],
  objectNodes :: [(String, Int)],
  decisionNodes :: [Int],
  mergeNodes :: [Int],
  forkNodes :: [Int],
  joinNodes :: [Int],
  initialNodes :: [Int],
  supportSTs :: [Int]
} deriving (Generic, Show, Eq, Read)

matchPetriSolution :: MatchPetriInstance -> MatchPetriSolution
matchPetriSolution task = mapTypesToLabels $ petrinet task

extractSupportSTs :: Net p n => p n PetriKey -> [PetriKey]
extractSupportSTs petri = filter
  (\x -> isSupportST x && not (isSinkST x petri))
  $ M.keys $ Petri.nodes petri

isSinkST :: Net p n => PetriKey -> p n PetriKey -> Bool
isSinkST key petri = M.null $ outFlow key petri

isSupportST :: PetriKey -> Bool
isSupportST key =
  case key of
    SupportST {} -> True
    _ -> False

matchPetriTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> MatchPetriInstance
  -> LangM m
matchPetriTask path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image $=<< liftIO
    $ drawADToFile path (plantUMLConf task) $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following Petri net."
    german "Betrachten Sie das folgende Petrinetz."
  let drawSetting = petriDrawConf task
  image $=<< fmap (failWith id) $ liftIO
    $ runExceptT
    $ cacheNet path (show . PK.label) (petrinet task)
      (not $ withPlaceNames drawSetting)
      (not $ withTransitionNames drawSetting)
      (not $ with1Weights drawSetting)
      (withGraphvizCommand drawSetting)
  paragraph $ translate $ do
    english [iii|
      State the matchings of each action node and Petri net node,
      the matching of each object node and Petri net node,
      the Petri net nodes per component type, as well as all support nodes
      of the Petri net.
      |]
    german [iii|
      Geben Sie alle Aktionsknoten/Petrinetzknotenpaare,
      Objektknoten/Petrinetzknotenpaare, die Petrinetzknoten pro Komponententyp
      und die Hilfsknoten im Petrinetz an.
      |]
  paragraph $ do
    translate $ do
      english [i|To do this, enter your answer as in the following example.|]
      german [i|Geben Sie dazu Ihre Antwort wie im folgenden Beispiel an.|]
    code $ show matchPetriInitial
    translate $ do
      english [i|In this example, the action nodes "A" and "B" are matched with the Petri net nodes 1 and 2,
the Petri net nodes 5 and 7 correspond to decision nodes and the Petri net nodes 13, 14 and 15 are support nodes.|]
      german [i|In diesem Beispiel sind etwa die Aktionsknoten "A" und "B" den Petrinetzknoten 1 und 2 zugeordnet,
die Petrinetzknoten 5 und 7 entsprechen mit Verzweigungsknoten und die Petrinetzknoten 13, 14 und 15 sind Hilfsknoten.|]
    pure ()
  pure ()

matchPetriInitial :: MatchPetriSolution
matchPetriInitial = MatchPetriSolution {
  actionNodes = [("A", 1), ("B", 2)],
  objectNodes = [("C", 3), ("D", 4)],
  decisionNodes = [5, 7],
  mergeNodes = [6, 8],
  forkNodes = [10],
  joinNodes = [11],
  initialNodes = [12],
  supportSTs = [13, 14, 15]
}

matchPetriSyntax
  :: (OutputMonad m)
  => MatchPetriInstance
  -> MatchPetriSolution
  -> LangM m
matchPetriSyntax task sub = addPretext $ do
  let adNames = map name $ filter (\n -> isActionNode n || isObjectNode n) $ nodes $ activityDiagram task
      subNames = map fst (actionNodes sub) ++ map fst (objectNodes sub)
      petriLabels = map PK.label $ M.keys $ allNodes $ petrinet task
      subLabels =
        map snd (actionNodes sub)
        ++ map snd (objectNodes sub)
        ++ decisionNodes sub
        ++ mergeNodes sub
        ++ forkNodes sub
        ++ joinNodes sub
        ++ initialNodes sub
        ++ supportSTs sub
  assertion (all (`elem` adNames) subNames) $ translate $ do
    english "Referenced node names were provided within task?"
    german "Referenzierte Knotennamen sind Bestandteil der Aufgabenstellung?"
  assertion (all (`elem` petriLabels) subLabels) $ translate $ do
    english "Referenced Petri net nodes were provided within task?"
    german "Referenzierte Petrinetzknoten sind Bestandteil der Aufgabenstellung?"
  pure ()

matchPetriEvaluation
  :: OutputMonad m
  => MatchPetriInstance
  -> MatchPetriSolution
  -> Rated m
matchPetriEvaluation task sub = addPretext $ do
  let as = translations $ do
        english "partial answers"
        german "Teilantworten"
      sol = matchPetriSolution task
      solution = matchPetriSolutionMap sol
      sub' = M.keys $ matchPetriSolutionMap sub
  multipleChoice as (Just $ show sol) solution sub'

matchPetriSolutionMap
  :: MatchPetriSolution
  -> Map (Int, Either [(String, Int)] [Int]) Bool
matchPetriSolutionMap sol =
  let xs = [
        Left $ sort $ actionNodes sol,
        Left $ sort $ objectNodes sol,
        Right $ sort $ decisionNodes sol,
        Right $ sort $ mergeNodes sol,
        Right $ sort $ forkNodes sol,
        Right $ sort $ joinNodes sol,
        Right $ sort $ initialNodes sol,
        Right $ sort $ supportSTs sol
        ]
  in M.fromList $ zipWith (curry (,True)) [1..] xs

matchPetri
  :: MatchPetriConfig
  -> Int
  -> Int
  -> IO MatchPetriInstance
matchPetri config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getMatchPetriTask config) g

getMatchPetriTask
  :: (RandomGen g, MonadIO m)
  => MatchPetriConfig
  -> RandT g m MatchPetriInstance
getMatchPetriTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ matchPetriAlloy config
  rinstas <- shuffleM instas
  activityDiagrams <- liftIO
    $ mapM (fmap snd . shuffleADNames . failWith id . parseInstance) rinstas
  let (ad, petri) = headWithErr "Failed to find task instances"
        $ filter (not . petriHasMultipleAutomorphisms . snd)
        $ map (second convertToPetrinet . dupe) activityDiagrams
  shuffledPetri <- liftIO $ snd <$> shufflePetri petri
  layout <- pickRandomLayout config
  return $ MatchPetriInstance {
    activityDiagram=ad,
    petrinet = shuffledPetri,
    plantUMLConf =
      PlantUMLConvConf {
        suppressNodeNames = False,
        suppressBranchConditions = hideBranchConditions config
      },
    petriDrawConf =
      DrawSettings {
        withPlaceNames = True,
        withTransitionNames = True,
        with1Weights = False,
        withGraphvizCommand = layout
      }
  }

defaultMatchPetriInstance :: MatchPetriInstance
defaultMatchPetriInstance = MatchPetriInstance
  { activityDiagram = UMLActivityDiagram
    { nodes =
      [ ADActionNode
        { label = 1 , name = "C" }
      , ADActionNode
        { label = 2
        , name = "H" }
      , ADActionNode
        { label = 3
        , name = "A" }
      , ADActionNode
        { label = 4
        , name = "E" }
      , ADObjectNode
        { label = 5
        , name = "F" }
      , ADObjectNode
        { label = 6
        , name = "D" }
      , ADObjectNode
        { label = 7
        , name = "G" }
      , ADObjectNode
        { label = 8
        , name = "B" }
      , ADDecisionNode
        { label = 9 }
      , ADDecisionNode
        { label = 10 }
      , ADMergeNode
        { label = 11 }
      , ADMergeNode
        { label = 12 }
      , ADForkNode
        { label = 13 }
      , ADJoinNode
        { label = 14 }
      , ADActivityFinalNode
        { label = 15 }
      , ADFlowFinalNode
        { label = 16 }
      , ADInitialNode
        { label = 17 } ]
    , connections =
      [ ADConnection
        { from = 1
        , to = 12
        , guard = "" }
      , ADConnection
        { from = 2
        , to = 6
        , guard = "" }
      , ADConnection
        { from = 3
        , to = 4
        , guard = "" }
      , ADConnection
        { from = 4
        , to = 13
        , guard = "" }
      , ADConnection
        { from = 5
        , to = 12
        , guard = "" }
      , ADConnection
        { from = 6
        , to = 14
        , guard = "" }
      , ADConnection
        { from = 7
        , to = 15
        , guard = "" }
      , ADConnection
        { from = 8
        , to = 7
        , guard = "" }
      , ADConnection
        { from = 9
        , to = 11
        , guard = "a" }
      , ADConnection
        { from = 9
        , to = 14
        , guard = "b" }
      , ADConnection
        { from = 10
        , to = 1
        , guard = "b" }
      , ADConnection
        { from = 10
        , to = 5
        , guard = "c" }
      , ADConnection
        { from = 11
        , to = 10
        , guard = "" }
      , ADConnection
        { from = 12
        , to = 9
        , guard = "" }
      , ADConnection
        { from = 13
        , to = 2
        , guard = "" }
      , ADConnection
        { from = 13
        , to = 8
        , guard = "" }
      , ADConnection
        { from = 13
        , to = 11
        , guard = "" }
      , ADConnection
        { from = 14
        , to = 16
        , guard = "" }
      , ADConnection
        { from = 17
        , to = 3
        , guard = "" } ] }
  , petrinet = PetriLike
    { allNodes = M.fromList
      [
        ( NormalST
          { label = 1
          , sourceNode = ADDecisionNode
            { label = 10 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 7
                , sourceNode = ADActionNode
                  { label = 1
                  , name = "C" } }
              , 1 )
            ,
              ( SupportST
                { label = 14 }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 2
          , sourceNode = ADJoinNode
            { label = 14 } }
        , SimpleTransition
          { flowOut = M.empty } )
      ,
        ( NormalST
          { label = 3
          , sourceNode = ADInitialNode
            { label = 17 } }
        , SimplePlace
          { initial = 1
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 19
                , sourceNode = ADActionNode
                  { label = 3
                  , name = "A" } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 4 }
        , SimpleTransition
          { flowOut = M.empty } )
      ,
        ( NormalST
          { label = 5
          , sourceNode = ADMergeNode
            { label = 11 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( SupportST
                { label = 21 }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 6
          , sourceNode = ADObjectNode
            { label = 6
            , name = "D" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 2
                , sourceNode = ADJoinNode
                  { label = 14 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 7
          , sourceNode = ADActionNode
            { label = 1
            , name = "C" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 13
                , sourceNode = ADMergeNode
                  { label = 12 } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 8 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 13
                , sourceNode = ADMergeNode
                  { label = 12 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 9
          , sourceNode = ADObjectNode
            { label = 7
            , name = "G" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( SupportST
                { label = 4 }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 10
          , sourceNode = ADActionNode
            { label = 2
            , name = "H" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 6
                , sourceNode = ADObjectNode
                  { label = 6
                  , name = "D" } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 11 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 12
                , sourceNode = ADForkNode
                  { label = 13 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 12
          , sourceNode = ADForkNode
            { label = 13 } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 5
                , sourceNode = ADMergeNode
                  { label = 11 } }
              , 1 )
            ,
              ( SupportST
                { label = 15 }
              , 1 )
            ,
              ( NormalST
                { label = 22
                , sourceNode = ADObjectNode
                  { label = 8
                  , name = "B" } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 13
          , sourceNode = ADMergeNode
            { label = 12 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( SupportST
                { label = 23 }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 14 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 25
                , sourceNode = ADObjectNode
                  { label = 5
                  , name = "F" } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 15 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 10
                , sourceNode = ADActionNode
                  { label = 2
                  , name = "H" } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 16
          , sourceNode = ADActionNode
            { label = 4
            , name = "E" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( SupportST
                { label = 11 }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 17 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 9
                , sourceNode = ADObjectNode
                  { label = 7
                  , name = "G" } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 18 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 5
                , sourceNode = ADMergeNode
                  { label = 11 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 19
          , sourceNode = ADActionNode
            { label = 3
            , name = "A" } }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( SupportST
                { label = 20 }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 20 }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 16
                , sourceNode = ADActionNode
                  { label = 4
                  , name = "E" } }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 21 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 1
                , sourceNode = ADDecisionNode
                  { label = 10 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 22
          , sourceNode = ADObjectNode
            { label = 8
            , name = "B" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( SupportST
                { label = 17 }
              , 1 ) ] } )
      ,
        ( SupportST
          { label = 23 }
        , SimpleTransition
          { flowOut = M.fromList
            [
              ( NormalST
                { label = 24
                , sourceNode = ADDecisionNode
                  { label = 9 } }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 24
          , sourceNode = ADDecisionNode
            { label = 9 } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( NormalST
                { label = 2
                , sourceNode = ADJoinNode
                  { label = 14 } }
              , 1 )
            ,
              ( SupportST
                { label = 18 }
              , 1 ) ] } )
      ,
        ( NormalST
          { label = 25
          , sourceNode = ADObjectNode
            { label = 5
            , name = "F" } }
        , SimplePlace
          { initial = 0
          , flowOut = M.fromList
            [
              ( SupportST
                { label = 8 }
              , 1 ) ] } ) ] }
  , plantUMLConf = defaultPlantUMLConvConf
  , petriDrawConf =
    DrawSettings {
      withPlaceNames = True,
      withTransitionNames = True,
      with1Weights = False,
      withGraphvizCommand = Dot
    }
  }
