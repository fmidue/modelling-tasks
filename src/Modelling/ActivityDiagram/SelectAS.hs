{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.ActivityDiagram.SelectAS (
  SelectASInstance(..),
  SelectASConfig(..),
  SelectASSolution(..),
  defaultSelectASConfig,
  checkSelectASConfig,
  selectASAlloy,
  checkSelectASInstance,
  selectActionSequence,
  selectASTask,
  selectASSyntax,
  selectASEvaluation,
  selectASSolution,
  selectAS,
  defaultSelectASInstance
) where

import qualified Data.Map as M (fromList, toList, keys, filter, map)
import qualified Data.Vector as V (fromList)

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Capabilities.WriteFile           (MonadWriteFile)
import Modelling.ActivityDiagram.ActionSequences (
  generateActionSequencesWithPetri,
  generateActionSequenceWithPetriAndRepetition,
  validActionSequenceWithPetri,
  netAndMap
  )
import Modelling.ActivityDiagram.Auxiliary.ActionSequences (actionSequencesAlloy)
import Modelling.ActivityDiagram.PetriNet (convertToPetriNet)
import Modelling.ActivityDiagram.Config (
  AdConfig (..),
  checkAdConfig,
  defaultAdConfig,
  )
import Modelling.ActivityDiagram.Datatype (
  AdConnection (..),
  AdNode (..),
  UMLActivityDiagram (..),
  )
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (
  PlantUmlConfig (..),
  defaultPlantUmlConfig,
  drawAdToFile,
  )
import Modelling.ActivityDiagram.Shuffle (shuffleAdNames)
import Modelling.Auxiliary.Common (
  TaskGenerationException (NoInstanceAvailable),
  )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Extra (firstJustM)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  ExtraText (..),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  extra,
  german,
  translate,
  translations,
  singleChoice,
  singleChoiceSyntax,
  )
import Control.Monad.Random (
  MonadRandom,
  RandT,
  RandomGen,
  uniform,
  evalRandT,
  mkStdGen
  )
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (permutations, sortBy)
import Data.List.Extra (groupOn, nubOrd)
import Data.Ord (comparing)
import Data.Map (Map)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate          (i, iii)
import Data.Vector.Distance (Params(..), leastChanges)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (
  addPretext,
  )
import System.Random.Shuffle (shuffleM)

data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  actionSequences :: Map Int (Bool, [String]),
  drawSettings :: PlantUmlConfig,
  showSolution :: Bool,
  addText :: ExtraText
}
  deriving (Eq, Generic, Hashable, Read, Reader, Show, ToDoc)

data SelectASConfig = SelectASConfig {
  adConfig :: AdConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  numberOfWrongAnswers :: Int,
  answerLength :: !(Int, Int),
  printSolution :: Bool,
  withActionRepetition :: Bool,
  extraText :: ExtraText
}
  deriving (Generic, Read, Reader, Show, ToDoc)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
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
  numberOfWrongAnswers = 2,
  answerLength = (5, 6),
  printSolution = True,
  withActionRepetition = False,
  extraText = NoExtraText
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkAdConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
    adConfig,
    maxInstances,
    objectNodeOnEveryPath,
    numberOfWrongAnswers,
    answerLength,
    withActionRepetition
  }
  | Just instances <- maxInstances, instances < 1
    = Just "The parameter 'maxInstances' must either be set to a positive value or to Nothing"
  | numberOfWrongAnswers < 1
    = Just "The parameter 'numberOfWrongAnswers' must be set to a positive value"
  | objectNodeOnEveryPath == Just True && fst (objectNodeLimits adConfig) < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occurring"
  | fst answerLength < 0
    = Just "The parameter 'answerLength' should not contain non-negative values"
  | uncurry (>) answerLength
  = Just [iii|
    The second value of parameter 'answerLength' should be greater or equal to
    its first value.
    |]
  | fst answerLength > 0 && fst (actionLimits adConfig) < 1
    = Just "If you want non-empty sequences, there must be action nodes in the first place."
  | withActionRepetition && cycles adConfig < 1
    = Just "Setting 'withActionRepetition' to True requires at least 1 cycle in the activity diagram configuration"
  | withActionRepetition && forkJoinPairs adConfig < 1
    = Just "Setting 'withActionRepetition' to True requires at least 1 fork/join pair in the activity diagram configuration"
  | withActionRepetition && fst answerLength < 2
    = Just "Setting 'withActionRepetition' to True requires sequences of at least 2 actions"
  | not withActionRepetition && snd answerLength > snd (actionLimits adConfig)
    = Just "Setting 'withActionRepetition' to False prevents sequences that are longer than action nodes exist"
  | not withActionRepetition && fst answerLength > fst (actionLimits adConfig)
    = Just "Setting 'withActionRepetition' to False means it doesn't make sense to have fewer action nodes than the minimum desired sequence length"
  | otherwise
    = Nothing

selectASAlloy :: SelectASConfig -> String
selectASAlloy SelectASConfig {
    adConfig,
    objectNodeOnEveryPath
  } = actionSequencesAlloy adConfig objectNodeOnEveryPath

checkSelectASInstance :: SelectASInstance -> Maybe String
checkSelectASInstance inst
  | suppressNodeNames (drawSettings inst)
  = Just "'suppressNodeNames' must be set to 'False' for this task type"
  | otherwise
  = Nothing


data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

{-|
Generate a set of one correct and multiple wrong sequences.
-}
selectActionSequence
  :: MonadRandom m
  => Bool
  -- ^ if sequences should contain at least one action twice
  -> Int
  -- ^ the number of wrong sequences to return
  -> (Int, Int)
  -- ^ how long the returned sequences should be
  -- specified by (lower, upper) bound
  -> UMLActivityDiagram
  -- ^ For which AD diagram the correct sequence should be valid
  -> MaybeT m SelectASSolution
selectActionSequence withRepetition numberOfWrongSequences lengthBounds ad = MaybeT $ do
  let petri = convertToPetriNet ad
  maybeCorrectSequence <- case (withRepetition, generateActionSequenceWithPetriAndRepetition petri lengthBounds) of
    (True, Just genAction) -> Just <$> genAction
    (True, Nothing) -> return Nothing
    (False, _) ->
      let
        validSequences = generateActionSequencesWithPetri ad petri (Just lengthBounds)
      in
        if null validSequences
        then
          return Nothing
        else
          Just <$> uniform validSequences
  case maybeCorrectSequence of
    Nothing -> return Nothing
    Just correctSequence -> do
      let (net, actionNameToPetriKey) = netAndMap petri
          allWrongCandidates =
            filter (\actionSeq -> not (validActionSequenceWithPetri actionSeq ad net actionNameToPetriKey)) $
            (if withRepetition then nubOrd else id) $
            permutations correctSequence
      -- Early check: reject if insufficient candidates
      if length allWrongCandidates < numberOfWrongSequences
        then return Nothing
        else do
          let -- Precompute edit distance parameters
              editDistParams = asEditDistParams correctSequence
              correctSeqVec = V.fromList correctSequence
              -- Pair each candidate with its distance
              candidatesWithDist = map (\actionSeq ->
                (actionSeq, getSum $ fst $ leastChanges editDistParams correctSeqVec (V.fromList actionSeq))) allWrongCandidates
              -- Sort by distance
              sortedByDist = sortBy (comparing snd) candidatesWithDist
              -- Group by distance
              groupedByDist = groupOn snd sortedByDist
              -- Determine how many groups we need
              (fullGroups, maybePartialGroup) = takeGroupsUntil numberOfWrongSequences groupedByDist
          -- Only shuffle the last group if it's partial, keep full groups as-is
          wrongSequences <- case maybePartialGroup of
            Nothing -> return fullGroups
            Just (numberLeft, lastGroup) -> do
              shuffledLast <- shuffleM lastGroup
              return (take numberLeft shuffledLast ++ fullGroups)
          return $ Just SelectASSolution {correctSequence = correctSequence, wrongSequences = wrongSequences}
  where
    -- Helper to take groups until we have enough elements
    -- Returns (fullGroupsWeNeed, maybePartialGroupToShuffle)
    takeGroupsUntil :: Int -> [[(a,b)]] -> ([a], Maybe (Int, [a]))
    takeGroupsUntil _ [] = ([], Nothing)
    takeGroupsUntil n (g:gs)
      | n <= 0 = ([], Nothing)
      | length g > n = ([], Just (n, map fst g))  -- This group is enough, needs shuffling
      | otherwise = let (rest, partial) = takeGroupsUntil (n - length g) gs
                    in (map fst g ++ rest, partial)

asEditDistParams :: [String] -> Params String (String, Int, String) (Sum Int)
asEditDistParams xs = Params
    { equivalent = (==)
    , delete     = \n s    -> ("delete", n, s)
    , insert     = \n s    -> ("insert", n, s)
    , substitute = \n _ s' -> ("replace", n, s')
    , cost = \ (_, n, _) -> Sum $ abs (n - (length xs `div` 2))
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

selectASTask
  :: (MonadPlantUml m, MonadWriteFile m, OutputCapable m)
  => FilePath
  -> SelectASInstance
  -> LangM m
selectASTask path task = do
  let mapping = M.toList $ M.map snd $ actionSequences task
  paragraph $ translate $ do
    english "Consider the following activity diagram:"
    german "Betrachten Sie folgendes Aktivitätsdiagramm:"
  image $=<< drawAdToFile path (drawSettings task) $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the sequences given here:"
    german "Betrachten Sie die hier gegebenen Folgen:"
  enumerateM (code . show) $ map (\(n,xs) -> (n, code $ show xs)) mapping
  paragraph $ translate $ do
    english [i|Which of these sequences is a valid action sequence?
State your answer by giving a number indicating the one valid action sequence among the above sequences.|]
    german [i|Welche dieser Folgen ist eine gültige Aktionsfolge?
Geben Sie Ihre Antwort als Zahl an, welche die eine gültige Aktionsfolge unter den obigen Folgen repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel würde|]
    code "2"
    translate $ do
      english [i|
        would indicate that sequence 2 is an executable sequence of action nodes.
        |]
      german  [i|
        bedeuten, dass Folge 2 eine ausführbare Folge von Aktionsknoten ist.
        |]
    pure ()
  extra $ addText task
  pure ()

selectASSolutionToMap
  :: (MonadRandom m)
  => SelectASSolution
  -> m (Map Int (Bool, [String]))
selectASSolutionToMap sol = do
  let xs = (True, correctSequence sol) : map (False, ) (wrongSequences sol)
  solution <- shuffleM xs
  return $ M.fromList $ zip [1..] solution

selectASSyntax
  :: OutputCapable m
  => SelectASInstance
  -> Int
  -> LangM m
selectASSyntax task sub = addPretext $ do
  let options = M.keys $ actionSequences task
  singleChoiceSyntax False options sub

selectASEvaluation
  :: OutputCapable m
  => SelectASInstance
  -> Int
  -> LangM m
selectASEvaluation task n = addPretext $ do
  let as = translations $ do
        english "action sequence"
        german "Aktionsfolge"
      solMap = actionSequences task
      (solution, validAS) = head $ M.toList $ M.map snd $ M.filter fst solMap
      solutionString =
        if showSolution task
        then Just . (DefiniteArticle,) $ show validAS
        else Nothing
  singleChoice as solutionString solution n

selectASSolution
  :: SelectASInstance
  -> Int
selectASSolution = head . M.keys . M.filter fst . actionSequences

selectAS
  :: (MonadAlloy m, MonadThrow m)
  => SelectASConfig
  -> Int
  -> Int
  -> m SelectASInstance
selectAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectASTask config) g

getSelectASTask
  :: (MonadAlloy m, MonadThrow m, RandomGen g)
  => SelectASConfig
  -> RandT g m SelectASInstance
getSelectASTask config = do
  instances <- lift $ getInstances
    (maxInstances config)
    Nothing
    $ selectASAlloy config
  randomInstances <- shuffleM instances >>= mapM (lift . parseInstance)
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances
  validInstances <- firstJustM (\x -> runMaybeT $ do
      solution <- selectActionSequence (withActionRepetition config) (numberOfWrongAnswers config) (answerLength config) x
      actionSequences <- lift $ selectASSolutionToMap solution
      return SelectASInstance {
            activityDiagram = x,
            actionSequences = actionSequences,
            drawSettings = defaultPlantUmlConfig {
              suppressBranchConditions = hideBranchConditions config
              },
            showSolution = printSolution config,
            addText = extraText config
          }
    ) ad
  case validInstances of
    Just x -> return x
    Nothing -> lift $ throwM NoInstanceAvailable

defaultSelectASInstance :: SelectASInstance
defaultSelectASInstance = SelectASInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      AdActionNode {label = 1, name = "E"},
      AdActionNode {label = 2, name = "D"},
      AdActionNode {label = 3, name = "A"},
      AdActionNode {label = 4, name = "C"},
      AdActionNode {label = 5, name = "F"},
      AdActionNode {label = 6, name = "B"},
      AdDecisionNode {label = 7},
      AdDecisionNode {label = 8},
      AdMergeNode {label = 9},
      AdMergeNode {label = 10},
      AdForkNode {label = 11},
      AdJoinNode {label = 12},
      AdFlowFinalNode {label = 13},
      AdFlowFinalNode {label = 14},
      AdInitialNode {label = 15}
    ],
    connections = [
      AdConnection {from = 1, to = 8, guard = ""},
      AdConnection {from = 2, to = 14, guard = ""},
      AdConnection {from = 3, to = 11, guard = ""},
      AdConnection {from = 4, to = 12, guard = ""},
      AdConnection {from = 5, to = 10, guard = ""},
      AdConnection {from = 6, to = 12, guard = ""},
      AdConnection {from = 7, to = 5, guard = "c"},
      AdConnection {from = 7, to = 9, guard = "a"},
      AdConnection {from = 8, to = 9, guard = "c"},
      AdConnection {from = 8, to = 10, guard = "b"},
      AdConnection {from = 9, to = 1, guard = ""},
      AdConnection {from = 10, to = 3, guard = ""},
      AdConnection {from = 11, to = 2, guard = ""},
      AdConnection {from = 11, to = 4, guard = ""},
      AdConnection {from = 11, to = 6, guard = ""},
      AdConnection {from = 12, to = 13, guard = ""},
      AdConnection {from = 15, to = 7, guard = ""}
    ]
  },
  actionSequences = M.fromList [
    (1, (False,["F","B","A","C","D"])),
    (2, (True,["F","A","B","C","D"])),
    (3, (False,["A","F","B","C","D"]))
    ],
  drawSettings = defaultPlantUmlConfig,
  showSolution = True,
  addText = NoExtraText
}
