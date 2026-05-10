{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.EnterAS (
  EnterASInstance(..),
  EnterASConfig(..),
  EnterASSolution(..),
  defaultEnterASConfig,
  checkEnterASConfig,
  enterASAlloy,
  checkEnterASInstance,
  enterActionSequence,
  enterASTask,
  enterASInitial,
  enterASSyntax,
  enterASEvaluation,
  enterASSolution,
  enterAS,
  defaultEnterASInstance
) where

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Capabilities.WriteFile           (MonadWriteFile)
import Modelling.ActivityDiagram.ActionSequences (
  generateActionSequencesWithPetri,
  netAndMap,
  computeActionSequenceLevels,
  getActionsLeadingToActivityFinals,
  isFinalPetriNode,
  )
import Modelling.ActivityDiagram.Auxiliary.ActionSequences (actionSequencesAlloy)
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  isActionNode,
  isObjectNode,
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PetriNet (
  PetriKey,
  convertToPetriNet,
  )
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)
import Modelling.Auxiliary.Common       (getFirstInstance)
import Modelling.PetriNet.Types         (Node, PetriLike)
import Modelling.PetriNet.Reach.Reach (isNoLonger)
import Modelling.PetriNet.Reach.Type (State(..), Net(start))

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Control.Monad.Catch              (MonadThrow)
import Control.Monad.Extra              (whenJust)
import Control.Monad.Trans.Class (lift)
import Control.OutputCapable.Blocks (
  ArticleToUse (IndefiniteArticle),
  ExtraText(..),
  GenericOutputCapable (..),
  LangM,
  Rated,
  OutputCapable,
  ($=<<),
  english,
  extra,
  german,
  translate,
  printSolutionAndAssert,
  yesNo,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  evalRandT,
  mkStdGen,
  )
import Data.List (intercalate, intersect)
import Data.List.Extra (nubOrd)
import qualified Data.Map as M (map)
import Data.Maybe                       (isNothing, isJust)
import Data.String.Interpolate (i, iii)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (
  addPretext,
  )
import System.Random.Shuffle (shuffleM)

data EnterASInstance = EnterASInstance {
  activityDiagram :: UMLActivityDiagram,
  petriNet :: PetriLike Node PetriKey,
  drawSettings :: PlantUmlConfig,
  sampleSequence :: [String],
  noLongerThan :: Maybe Int,
  showSolution :: Bool,
  addText :: ExtraText
}
  deriving (Eq, Generic, Hashable, Read, Reader, Show, ToDoc)

data EnterASConfig = EnterASConfig {
  adConfig :: AdConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  answerLength :: !(Int, Int),
  rejectLongerThan :: Maybe Int,
  printSolution :: Bool,
  extraText :: ExtraText
}
  deriving (Generic, Read, Reader, Show, ToDoc)


defaultEnterASConfig :: EnterASConfig
defaultEnterASConfig = EnterASConfig {
  adConfig = defaultAdConfig {
    actionLimits = (6, 6),
    objectNodeLimits = (1, 1),
    maxNamedNodes = 7,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
  answerLength = (5, 8),
  rejectLongerThan = Nothing,
  printSolution = True,
  extraText = NoExtraText
}

checkEnterASConfig :: EnterASConfig -> Maybe String
checkEnterASConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkEnterASConfig' conf

checkEnterASConfig' :: EnterASConfig -> Maybe String
checkEnterASConfig' EnterASConfig {
    adConfig,
    maxInstances,
    objectNodeOnEveryPath,
    answerLength,
    rejectLongerThan
  }
  | Just instances <- maxInstances, instances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | objectNodeOnEveryPath == Just True && fst (objectNodeLimits adConfig) < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occurring"
  | fst answerLength < 0
  = Just "The parameter 'answerLength' should not contain non-negative values"
  | uncurry (>) answerLength
  = Just [iii|
    The second value of parameter 'answerLength'
    should be greater than or equal to its first value.
    |]
  | maybe False (snd answerLength >) rejectLongerThan
  = Just [iii|
    'rejectLongerThan' should be greater than or equal to the second value of 'answerLength'
    |]
  | otherwise
    = Nothing

enterASAlloy :: EnterASConfig -> String
enterASAlloy EnterASConfig {
    adConfig,
    objectNodeOnEveryPath
  } = actionSequencesAlloy adConfig objectNodeOnEveryPath

checkEnterASInstance :: EnterASInstance -> Maybe String
checkEnterASInstance inst
  | suppressNodeNames (drawSettings inst)
  = Just "'suppressNodeNames' must be set to 'False' for this task type"
  | otherwise
  = Nothing

checkEnterASInstanceForConfig :: EnterASInstance -> EnterASConfig -> Maybe String
checkEnterASInstanceForConfig inst EnterASConfig {
  answerLength
  }
  | solutionLength < fst answerLength
  = Just [iii|
    Solution should not be shorter than
    the first value of parameter 'answerLength'.
    |]
  | solutionLength > snd answerLength
  = Just [iii|
    Solution should not be longer than
    the second value of parameter 'answerLength'.
    |]
  | otherwise
    = Nothing
  where solutionLength = length $ sampleSequence inst

newtype EnterASSolution = EnterASSolution {
  sampleSolution :: [String]
} deriving (Show, Eq)

enterActionSequence :: UMLActivityDiagram -> PetriLike Node PetriKey -> EnterASSolution
enterActionSequence ad petri =
  EnterASSolution {sampleSolution = head $ generateActionSequencesWithPetri ad petri Nothing}

enterASTask
  :: (MonadPlantUml m, MonadWriteFile m, OutputCapable m)
  => Bool
  -> FilePath
  -> EnterASInstance
  -> LangM m
enterASTask showInputHelp path task = do
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (drawSettings task) $ activityDiagram task
  paragraph $ do
    translate $ do
      english [iii|
        State the action sequence (i.e., a sequence of action nodes)
        of an execution of this diagram which lets all flows terminate.|]
      german [iii|
        Geben Sie die Aktionsfolge (d.h., eine Folge von Aktionsknoten)
        eines Ablaufs dieses Diagramms an, welcher alle Flüsse terminieren lässt.|]
    when showInputHelp $ do
     translate $ do
      english [i|
        State your answer by entering a list of action names.
        \n
        For example, |]
      german [i|
        Geben Sie Ihre Antwort an, indem Sie eine Liste von Aktionsnamen eingeben.
        \n
        Zum Beispiel drückt |]
     code $ show enterASInitial
     translate $ do
      english [i|expresses the execution of A followed by B (under the assumption that both are action nodes of the diagram).|]
      german [i|die Ausführung von A gefolgt von B aus (unter der Annahme, dass beides Aktionsknoten des Diagramms sind).|]
     pure ()
    pure ()
  whenJust (noLongerThan task) $ \maxL -> paragraph $ translate $ do
    english $ "Your answer must not exceed " ++ show maxL ++ " steps."
    german $ "Ihre Antwort darf maximal " ++ show maxL ++ " Schritte enthalten."
  extra $ addText task
  pure ()

enterASInitial :: [String]
enterASInitial = ["A", "B"]

enterASSyntax
  :: OutputCapable m
  => EnterASInstance
  -> [String]
  -> LangM m
enterASSyntax task sub = addPretext $ do
  let adNames = map name
        $ filter (\n -> isActionNode n || isObjectNode n)
        $ nodes
        $ activityDiagram task
  assertion (all (`elem` adNames) sub) $ translate $ do
    english "Referenced node names are part of the given activity diagram?"
    german "Referenzierte Knotennamen sind Bestandteil des gegebenen Aktivitätsdiagramms?"
  isNoLonger (noLongerThan task) sub
  pure ()

enterASEvaluation
  :: OutputCapable m
  => EnterASInstance
  -> [String]
  -> Rated m
enterASEvaluation task sub = do
  let diag = activityDiagram task
      objectNames = map name $ filter isObjectNode $ nodes diag
      objectNamesInSubmission = nubOrd $ sub `intersect` objectNames
      (net, actionNameToPetriKey) = netAndMap (petriNet task)
      zeroState = State $ M.map (const 0) $ unState $ start net
      levels = computeActionSequenceLevels sub net actionNameToPetriKey (getActionsLeadingToActivityFinals diag)
      reachesZeroState = any (isJust . lookup zeroState) levels
      correct = null objectNamesInSubmission && reachesZeroState
      points = if correct then 1 else 0
      maybeSolutionString =
        if showSolution task
        then Just . (IndefiniteArticle,) $ show $ sampleSequence task
        else Nothing

  yesNo correct $ translate $ do
    english "The submitted node sequence is correct?"
    german "Die eingereichte Knotenfolge ist korrekt?"

  -- Provide specific feedback for sequences that terminate some but not all flows
  when (null objectNamesInSubmission && not reachesZeroState) $ do
    let finalNodeReached = any (any (\(_, path) -> any isFinalPetriNode path)) levels
    when finalNodeReached $ do
      paragraph $ translate $ do
        german [iii|
          Mit der eingereichten Sequenz wird ein Flussende erreicht, aber sie terminiert nicht alle Flüsse.
          Beachten Sie, dass das Erreichen eines Flussendes nur den hineinlaufenden Kontrollfluss beendet,
          während andere Flüsse (z.B. nach Aufspaltung an einem Fork-Knoten) weiterhin aktiv bleiben können.
          Eine korrekte Lösung muss alle im Ablauf befindlichen Flüsse terminieren.
          |]
        english [iii|
          With the submitted sequence a flow final node is reached, but it does not terminate all flows.
          Note that reaching a flow final node only terminates the incoming control flow,
          while other flows (e.g., after splitting at a fork node) may remain active.
          A correct solution must terminate all flows under execution.
          |]
      pure ()

  unless (null objectNamesInSubmission) $ do
    translate $ do
      english "The following referenced nodes are object nodes and thus not actions:"
      german "Die folgenden referenzierten Knoten sind Objektknoten und damit keine Aktionen:"
    code $ intercalate ", " objectNamesInSubmission
    pure ()

  printSolutionAndAssert False maybeSolutionString points

  pure points

enterASSolution
  :: EnterASInstance
  -> [String]
enterASSolution = sampleSequence

enterAS
  :: (MonadAlloy m, MonadThrow m)
  => EnterASConfig
  -> Int
  -> Int
  -> m EnterASInstance
enterAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getEnterASTask config) g

getEnterASTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => EnterASConfig
  -> RandT g m EnterASInstance
getEnterASTask config = do
  alloyInstances <- lift $ getInstances
    (maxInstances config)
    Nothing
    $ enterASAlloy config
  randomInstances <- shuffleM alloyInstances >>= mapM (lift . parseInstance)
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances
  lift $ getFirstInstance
        $ filter (isNothing . (`checkEnterASInstanceForConfig` config))
        $ map (\x -> let petri = convertToPetriNet x
                     in EnterASInstance {
          activityDiagram=x,
          petriNet=petri,
          drawSettings = defaultPlantUmlConfig {
            suppressBranchConditions = hideBranchConditions config
            },
          sampleSequence = sampleSolution $ enterActionSequence x petri,
          noLongerThan = rejectLongerThan config,
          showSolution = printSolution config,
          addText = extraText config
        }) ad

defaultEnterASInstance :: EnterASInstance
defaultEnterASInstance =
 let
  ad = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "A"},
      AdActionNode {label = 2, name = "E"},
      AdActionNode {label = 3, name = "F"},
      AdActionNode {label = 4, name = "G"},
      AdActionNode {label = 5, name = "D"},
      AdActionNode {label = 6, name = "B"},
      AdObjectNode {label = 7, name = "C"},
      AdDecisionNode {label = 8},
      AdDecisionNode {label = 9},
      AdMergeNode {label = 10},
      AdMergeNode {label = 11},
      AdForkNode {label = 12},
      AdJoinNode {label = 13},
      AdFlowFinalNode {label = 14},
      AdFlowFinalNode {label = 15},
      AdInitialNode {label = 16}
    ],
    connections = [
      AdConnection {from = 1, to = 10, guard = ""},
      AdConnection {from = 2, to = 13, guard = ""},
      AdConnection {from = 3, to = 10, guard = ""},
      AdConnection {from = 4, to = 8, guard = ""},
      AdConnection {from = 5, to = 12, guard = ""},
      AdConnection {from = 6, to = 9, guard = ""},
      AdConnection {from = 7, to = 5, guard = ""},
      AdConnection {from = 8, to = 11, guard = "a"},
      AdConnection {from = 8, to = 13, guard = "b"},
      AdConnection {from = 9, to = 1, guard = "a"},
      AdConnection {from = 9, to = 3, guard = "b"},
      AdConnection {from = 10, to = 15, guard = ""},
      AdConnection {from = 11, to = 4, guard = ""},
      AdConnection {from = 12, to = 2, guard = ""},
      AdConnection {from = 12, to = 6, guard = ""},
      AdConnection {from = 12, to = 11, guard = ""},
      AdConnection {from = 13, to = 14, guard = ""},
      AdConnection {from = 16, to = 7, guard = ""}
    ]
  }
 in
  EnterASInstance {
  activityDiagram = ad,
  petriNet = convertToPetriNet ad,
  drawSettings = defaultPlantUmlConfig,
  sampleSequence = ["D","E","G","B","F"],
  noLongerThan = Nothing,
  showSolution = True,
  addText = NoExtraText
}
