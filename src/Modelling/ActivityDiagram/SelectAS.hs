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
  selectAS,
  defaultSelectASInstance
) where

import qualified Data.Map as M (fromList, toList, keys, filter, map)
import qualified Data.Vector as V (fromList)

import Modelling.ActivityDiagram.ActionSequences (generateActionSequence, validActionSequence)
import Modelling.ActivityDiagram.Alloy (moduleActionSequencesRules)
import Modelling.ActivityDiagram.Config (ADConfig(..), defaultADConfig, checkADConfig, adConfigToAlloy)
import Modelling.ActivityDiagram.Datatype (UMLActivityDiagram(..), ADNode(..), ADConnection(..))
import Modelling.ActivityDiagram.Instance (parseInstance)
import Modelling.ActivityDiagram.PlantUMLConverter (drawADToFile, defaultPlantUMLConvConf)
import Modelling.ActivityDiagram.Shuffle (shuffleADNames)

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
import Data.List (permutations, sortBy)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Monoid (Sum(..), getSum)
import Data.String.Interpolate ( i )
import Data.Vector.Distance (Params(..), leastChanges)
import Language.Alloy.Call (getInstances)
import Modelling.Auxiliary.Output (addPretext)
import System.Random.Shuffle (shuffle', shuffleM)


data SelectASInstance = SelectASInstance {
  activityDiagram :: UMLActivityDiagram,
  seed :: Int,
  actionSequences :: Map Int (Bool, [String])
} deriving (Show, Eq)

data SelectASConfig = SelectASConfig {
  adConfig :: ADConfig,
  maxInstances :: Maybe Integer,
  objectNodeOnEveryPath :: Maybe Bool,
  numberOfWrongAnswers :: Int,
  minAnswerLength :: Int,
  maxAnswerLength :: Int
} deriving (Show)

defaultSelectASConfig :: SelectASConfig
defaultSelectASConfig = SelectASConfig {
  adConfig = defaultADConfig {
    minActions = 6,
    maxActions = 8,
    minObjectNodes = 0,
    maxObjectNodes = 0,
    activityFinalNodes = 0,
    flowFinalNodes = 2
  },
  maxInstances = Just 50,
  objectNodeOnEveryPath = Nothing,
  numberOfWrongAnswers = 2,
  minAnswerLength = 5,
  maxAnswerLength = 8
}

checkSelectASConfig :: SelectASConfig -> Maybe String
checkSelectASConfig conf =
  checkADConfig (adConfig conf)
  <|> checkSelectASConfig' conf

checkSelectASConfig' :: SelectASConfig -> Maybe String
checkSelectASConfig' SelectASConfig {
    adConfig,
    objectNodeOnEveryPath,
    minAnswerLength,
    maxAnswerLength
  }
  | objectNodeOnEveryPath == Just True && minObjectNodes adConfig < 1
    = Just "Setting the parameter 'objectNodeOnEveryPath' to True implies at least 1 Object Node occuring"
  | minAnswerLength < 0
    = Just "The parameter 'minAnswerLength' should be non-negative"
  | maxAnswerLength < minAnswerLength
    = Just "The parameter 'maxAnswerLength' should be greater or equal to 'minAnswerLength'"
  | otherwise
    = Nothing

selectASAlloy :: SelectASConfig -> String
selectASAlloy SelectASConfig {
    adConfig,
    objectNodeOnEveryPath
  }
  = adConfigToAlloy modules preds adConfig
  where modules = moduleActionSequencesRules
        preds =
          [i|
            noActivityFinalNodes
            someActionNodesExistInEachBlock
            #{f objectNodeOnEveryPath "checkIfStudentKnowsDifferenceBetweenObjectAndActionNodes"}
          |]
        f opt s =
          case opt of
            Just True -> s
            Just False -> [i| not #{s}|]
            _ -> ""

checkSelectASInstance :: SelectASInstance -> SelectASConfig -> Maybe String
checkSelectASInstance inst SelectASConfig {
    minAnswerLength,
    maxAnswerLength
  }
  | length solution < minAnswerLength
    = Just "Solution should not be shorter than parameter 'minAnswerLength'"
  | length solution > maxAnswerLength
    = Just "Solution should not be longer than parameter 'maxAnswerLength'"
  | otherwise
    = Nothing
  where (_, solution) = head $ M.toList $ M.map snd $ M.filter fst $ actionSequences inst

data SelectASSolution = SelectASSolution {
  correctSequence :: [String],
  wrongSequences :: [[String]]
} deriving (Show, Eq)

selectActionSequence :: Int -> UMLActivityDiagram -> SelectASSolution
selectActionSequence numberOfWrongSequences ad =
  let correctSequence = generateActionSequence ad
      wrongSequences =
        take numberOfWrongSequences $
        sortBy (compareDistToCorrect correctSequence) $
        filter (not . (`validActionSequence` ad)) $
        permutations correctSequence
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

selectASTask
  :: (OutputMonad m, MonadIO m)
  => FilePath
  -> SelectASInstance
  -> LangM m
selectASTask path task = do
  let mapping = M.toList $ M.map snd $ actionSequences task
  ad <- liftIO $ drawADToFile path defaultPlantUMLConvConf $ activityDiagram task
  paragraph $ translate $ do
    english "Consider the following activity diagram."
    german "Betrachten Sie das folgende Aktivitätsdiagramm."
  image ad
  paragraph $ translate $ do
    english "Consider the following sequences."
    german "Betrachten Sie die folgenden Folgen."
  enumerateM (code . show) $ map (\(n,xs) -> (n, code $ show xs)) mapping
  paragraph $ translate $ do
    english [i|Which of these sequences is a valid action sequences?
Please state your answer by giving a number indicating the valid action sequence.|]
    german [i|Welcher dieser Folgen ist eine valide Aktionsfolge?
Bitte geben Sie ihre Antwort als Zahl an, welche die valide Aktionsfolge repräsentiert.|]
  paragraph $ do
    translate $ do
      english [i|For example,|]
      german [i|Zum Beispiel|]
    code "2"
    translate $ do
      english [i|would indicate that sequence 2 is the valid action sequence.|]
      german  [i|würde bedeuten, dass Folge 2 die valide Aktionsfolge ist.|]

selectASSolutionToMap
  :: Int
  -> SelectASSolution
  -> Map Int (Bool, [String])
selectASSolutionToMap seed sol =
  let xs = (True, correctSequence sol) : map (False, ) (wrongSequences sol)
      solution = shuffle' xs (length xs) (mkStdGen seed)
  in M.fromList $ zip [1..] solution

selectASSyntax
  :: (OutputMonad m)
  => SelectASInstance
  -> Int
  -> LangM m
selectASSyntax task sub = addPretext $ do
  let options = M.keys $ actionSequences task
  singleChoiceSyntax True options sub

selectASEvaluation
  :: OutputMonad m
  => SelectASInstance
  -> Int
  -> Rated m
selectASEvaluation task n = addPretext $ do
  let as = translations $ do
        english "action sequence"
        german "Aktionsfolge"
      solMap = actionSequences task
      (solution, validAS) = head $ M.toList $ M.map snd $ M.filter fst solMap
  singleChoice as (Just $ show validAS) solution n

selectAS
  :: SelectASConfig
  -> Int
  -> Int
  -> IO SelectASInstance
selectAS config segment seed = do
  let g = mkStdGen $ (segment +) $ 4 * seed
  evalRandT (getSelectASTask config) g

getSelectASTask
  :: (RandomGen g, MonadIO m)
  => SelectASConfig
  -> RandT g m SelectASInstance
getSelectASTask config = do
  instas <- liftIO $ getInstances (maxInstances config) $ selectASAlloy config
  rinstas <- shuffleM instas
  n <- getRandom
  g' <- getRandom
  let ad = map (snd . shuffleADNames n . failWith id . parseInstance) rinstas
      validInsta = head
        $ filter (isNothing . (`checkSelectASInstance` config))
        $ map (\x ->
          SelectASInstance {
            activityDiagram=x,
            seed=g',
            actionSequences=selectASSolutionToMap g'
              $ selectActionSequence (numberOfWrongAnswers config) x
          }) ad
  return validInsta

failWith :: (a -> String) -> Either a c -> c
failWith f = either (error . f) id

defaultSelectASInstance :: SelectASInstance
defaultSelectASInstance = SelectASInstance {
  activityDiagram = UMLActivityDiagram {
    nodes = [
      ADActionNode {label = 1, name = "E"},
      ADActionNode {label = 2, name = "D"},
      ADActionNode {label = 3, name = "A"},
      ADActionNode {label = 4, name = "C"},
      ADActionNode {label = 5, name = "F"},
      ADActionNode {label = 6, name = "B"},
      ADDecisionNode {label = 7},
      ADDecisionNode {label = 8},
      ADMergeNode {label = 9},
      ADMergeNode {label = 10},
      ADForkNode {label = 11},
      ADJoinNode {label = 12},
      ADFlowFinalNode {label = 13},
      ADFlowFinalNode {label = 14},
      ADInitialNode {label = 15}
    ],
    connections = [
      ADConnection {from = 1, to = 8, guard = ""},
      ADConnection {from = 2, to = 14, guard = ""},
      ADConnection {from = 3, to = 11, guard = ""},
      ADConnection {from = 4, to = 12, guard = ""},
      ADConnection {from = 5, to = 10, guard = ""},
      ADConnection {from = 6, to = 12, guard = ""},
      ADConnection {from = 7, to = 5, guard = "c"},
      ADConnection {from = 7, to = 9, guard = "a"},
      ADConnection {from = 8, to = 9, guard = "c"},
      ADConnection {from = 8, to = 10, guard = "b"},
      ADConnection {from = 9, to = 1, guard = ""},
      ADConnection {from = 10, to = 3, guard = ""},
      ADConnection {from = 11, to = 2, guard = ""},
      ADConnection {from = 11, to = 4, guard = ""},
      ADConnection {from = 11, to = 6, guard = ""},
      ADConnection {from = 12, to = 13, guard = ""},
      ADConnection {from = 15, to = 7, guard = ""}
    ]
  },
  seed = -4748947987859297750,
  actionSequences = M.fromList [(1,(False,["F","B","A","C","D"])),(2,(True,["F","A","B","C","D"])),(3,(False,["A","F","B","C","D"]))]
}