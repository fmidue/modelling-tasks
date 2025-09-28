{-# LANGUAGE ApplicativeDo #-}
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

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.PlantUml            (MonadPlantUml)
import Capabilities.WriteFile           (MonadWriteFile)
import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
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
import Control.Monad.Extra (firstJustM)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  ($=<<),
  english,
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
  evalRandT,
  mkStdGen
  )
import Data.List (permutations, sortBy)
import Data.Map (Map)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate          (i, iii)
import Data.Vector.Distance (Params(..), leastChanges)
import GHC.Generics (Generic)
import Modelling.Auxiliary.Output (
  addPretext,
  extra
  )
import System.Random.Shuffle (shuffleM)

data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  actionSequences :: Map Int (Bool, [String]),
  drawSettings :: PlantUmlConfig,
  showSolution :: Bool,
  addText :: Maybe (Map Language String)
} deriving (Eq, Generic, Read, Show)

data SelectASConfig = SelectASConfig {
  adConfig :: AdConfig,
  hideBranchConditions :: Bool,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  numberOfWrongAnswers :: Int,
  answerLength :: !(Int, Int),
  printSolution :: Bool,
  requireActionDuplication :: Maybe Bool,
  extraText :: Maybe (Map Language String)
} deriving (Generic, Read, Show)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
  adConfig = defaultAdConfig {
    actionLimits = (6, 8),
    objectNodeLimits = (1, 3),
    maxNamedNodes = 7,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  hideBranchConditions = True,
  maxInstances = Just 50,
  objectNodeOnEveryPath = Just True,
  numberOfWrongAnswers = 2,
  answerLength = (5, 8),
  printSolution = False,
  requireActionDuplication = Nothing,
  extraText = Nothing
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
    requireActionDuplication
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
  | requireActionDuplication == Just True && cycles adConfig == 0
    = Just "Setting the parameter 'requireActionDuplication' to True requires at least 1 cycle in the diagram"
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

checkSelectASInstanceForConfig
  :: SelectASInstance
  -> SelectASConfig
  -> Maybe String
checkSelectASInstanceForConfig inst SelectASConfig {
  answerLength
  }
  | length solution < fst answerLength
  = Just "Solution should not be shorter than the minimal 'answerLength'"
  | length solution > snd answerLength
  = Just "Solution should not be longer than the maximal 'answerLength'"
  | otherwise
    = Nothing
  where (_, solution) = head $ M.toList $ M.map snd $ M.filter fst $ actionSequences inst

data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

selectActionSequence :: Int -> Maybe Bool -> UMLActivityDiagram -> SelectASSolution
selectActionSequence numberOfWrongSequences requireActionDuplication ad =
  let baseCorrectSequence = generateActionSequence ad
      correctSequence = case requireActionDuplication of
        Just True -> generateCorrectSequenceWithDuplication baseCorrectSequence ad
        _ -> baseCorrectSequence
      wrongSequences =
        take numberOfWrongSequences $
        sortBy (compareDistToCorrect correctSequence) $
        filter (not . (`validActionSequence` ad)) $
        case requireActionDuplication of
          Just True -> generateSequencesWithDuplication correctSequence ad
          _ -> permutations correctSequence
  in SelectASSolution {correctSequence=correctSequence, wrongSequences=wrongSequences}

asEditDistParams :: [String] -> Params String (String, Int, String) (Sum Int)
asEditDistParams xs = Params
    { equivalent = (==)
    , delete     = \n s    -> ("delete", n, s)
    , insert     = \n s    -> ("insert", n, s)
    , substitute = \n _ s' -> ("replace", n, s')
    , cost = \ (_, n, _) -> Sum $ abs (n - (length xs `div` 2))
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

compareDistToCorrect :: [String] -> [String] -> [String] -> Ordering
compareDistToCorrect correctSequence xs ys =
  compare (distToCorrect xs) (distToCorrect ys)
  where
    distToCorrect zs =
      getSum
      $ fst
      $ leastChanges (asEditDistParams correctSequence) (V.fromList correctSequence) (V.fromList zs)

-- | Generate a correct sequence with action duplication when cycles exist
generateCorrectSequenceWithDuplication :: [String] -> UMLActivityDiagram -> [String]
generateCorrectSequenceWithDuplication baseSequence ad =
  let availableActions = map name $ filter isActionNode $ nodes ad
      -- Try multiple strategies for duplication: single actions and subsequences
      candidateSequences =
        singleActionDuplicates ++ subsequenceDuplicates ++ cyclicExtensions

      -- Strategy 1: Duplicate single actions at various positions
      singleActionDuplicates =
        [ extendedSeq
        | action <- availableActions
        , action `elem` baseSequence  -- Only duplicate actions that exist
        , pos <- [0..length baseSequence]  -- Insert at any position including end
        , let extendedSeq = insertActionAt pos action baseSequence
        , validActionSequence extendedSeq ad  -- Must be valid
        ]

      -- Strategy 2: Duplicate subsequences of the base sequence
      subsequenceDuplicates =
        [ baseSequence ++ subseq
        | len <- [2, 3]  -- Try subsequences of length 2 and 3
        , len <= length baseSequence
        , start <- [0..length baseSequence - len]
        , let subseq = take len $ drop start baseSequence
        , validActionSequence (baseSequence ++ subseq) ad
        ]

      -- Strategy 3: Try inserting subsequences at different positions
      cyclicExtensions =
        [ insertSubsequenceAt pos subseq baseSequence
        | len <- [2]  -- Try subsequences of length 2
        , len <= length baseSequence
        , start <- [0..length baseSequence - len]
        , let subseq = take len $ drop start baseSequence
        , pos <- [0..length baseSequence]
        , let extended = insertSubsequenceAt pos subseq baseSequence
        , validActionSequence extended ad
        ]

  in case candidateSequences of
       (validExtended:_) -> validExtended  -- Return first valid extended sequence
       [] -> baseSequence  -- Fall back to base sequence if no valid extension found
  where
    -- Insert an action at a specific position in the sequence
    insertActionAt :: Int -> String -> [String] -> [String]
    insertActionAt pos action actionSeq =
      let (before, after) = splitAt pos actionSeq
      in before ++ [action] ++ after

    -- Insert a subsequence at a specific position in the sequence
    insertSubsequenceAt :: Int -> [String] -> [String] -> [String]
    insertSubsequenceAt pos subseq actionSeq =
      let (before, after) = splitAt pos actionSeq
      in before ++ subseq ++ after

-- | Generate sequences with potential action duplication for cycles
generateSequencesWithDuplication :: [String] -> UMLActivityDiagram -> [[String]]
generateSequencesWithDuplication correctSequence ad =
  let availableActions = map name $ filter isActionNode $ nodes ad
      -- Generate sequences with guaranteed duplications first
      duplicatedSequences = concatMap (generateGuaranteedDuplication availableActions) [1..2]
      -- Generate sequences by inserting duplicated actions from cycles
      moreDuplicatedSequences = concatMap (generateWithDuplication availableActions) [1..2]
      -- Also include regular permutations for variety
      permutedSequences = permutations correctSequence
  in duplicatedSequences ++ moreDuplicatedSequences ++ permutedSequences
  where
    -- Generate sequences that are guaranteed to have duplications
    generateGuaranteedDuplication :: [String] -> Int -> [[String]]
    generateGuaranteedDuplication actions numDuplicates =
      [ correctSequence ++ replicate numDuplicates action
      | action <- take 2 actions  -- Limit to avoid too many sequences
      , action `elem` correctSequence
      ]

    -- Generate sequences by duplicating actions at various positions
    generateWithDuplication :: [String] -> Int -> [[String]]
    generateWithDuplication actions numDuplicates =
      [ insertDuplicates actionSeq action numDuplicates
      | actionSeq <- take 3 (permutations correctSequence)  -- Limit permutations
      , action <- take 2 actions  -- Limit actions to process
      , action `elem` actionSeq  -- Only duplicate actions that exist in correct sequence
      ]

    -- Insert duplicates of an action at valid positions in the sequence
    insertDuplicates :: [String] -> String -> Int -> [String]
    insertDuplicates actionSeq action numDups =
      let positions = [j | (j, x) <- zip [0..] actionSeq, x == action]
      in if null positions
         then actionSeq
         else let pos = head positions
              in take (pos + 1) actionSeq ++ replicate numDups action ++ drop (pos + 1) actionSeq

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
        then Just $ show validAS
        else Nothing
  singleChoice DefiniteArticle as solutionString solution n

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
  instances <- getInstances
    (maxInstances config)
    Nothing
    $ selectASAlloy config
  randomInstances <- shuffleM instances >>= mapM parseInstance
  ad <- mapM (fmap snd . shuffleAdNames) randomInstances
  validInstances <- firstJustM (\x -> do
    actionSequences <- selectASSolutionToMap $ selectActionSequence (numberOfWrongAnswers config) (requireActionDuplication config) x
    let selectASInst = SelectASInstance {
          activityDiagram=x,
          actionSequences = actionSequences,
          drawSettings = defaultPlantUmlConfig {
            suppressBranchConditions = hideBranchConditions config
            },
          showSolution = printSolution config,
          addText = extraText config
        }
    case checkSelectASInstanceForConfig selectASInst config of
      Just _ -> return Nothing
      Nothing -> return $ Just selectASInst
    ) ad
  case validInstances of
    Just x -> return x
    Nothing -> throwM NoInstanceAvailable

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
  showSolution = False,
  addText = Nothing
}
