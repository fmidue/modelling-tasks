{-# LANGUAGE DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.ConcurrencyAndConflict (
  FindInstance (..),
  PickInstance (..),
  checkFindConcurrencyConfig, checkFindConflictConfig,
  checkPickConcurrencyConfig, checkPickConflictConfig,
  findConcurrency,
  findConcurrencyEvaluation,
  findConcurrencyGenerate,
  findConcurrencyTask,
  findConflict,
  findConflictEvaluation,
  findConflictGenerate,
  findConflictTask,
  findTaskInstance,
  parseConcurrency,
  parseConflict,
  petriNetFindConcur, petriNetFindConfl,
  petriNetPickConcur, petriNetPickConfl,
  pickConcurrency,
  pickConcurrencyGenerate,
  pickConcurrencyTask,
  pickConflict,
  pickConflictGenerate,
  pickConflictTask,
  pickEvaluation,
  pickTaskInstance,
  ) where

import qualified Data.Bimap                       as BM (fromList, lookup)
import qualified Data.Map                         as M (
  elems,
  filter,
  fromList,
  keys,
  foldrWithKey,
  insert,
  )
import qualified Data.Set                         as Set (
  Set,
  toList,
  )

import Modelling.Auxiliary.Output (
  LangM',
  LangM,
  OutputMonad (..),
  english,
  singleChoice,
  )
import Modelling.PetriNet.Alloy (
  compAdvConstraints,
  compBasicConstraints,
  compChange,
  connected,
  isolated,
  moduleHelpers,
  modulePetriAdditions,
  modulePetriConcepts,
  modulePetriConstraints,
  modulePetriSignature,
  petriScopeBitwidth,
  petriScopeMaxSeq,
  taskInstance,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind, checkConfigForPick,
  )
import Modelling.PetriNet.Diagram       (drawNet, getDefaultNet, getNet)
import Modelling.PetriNet.Parser        (
  asSingleton,
  )
import Modelling.PetriNet.Types         (
  AdvConfig,
  BasicConfig (..),
  ChangeConfig,
  Concurrent (Concurrent),
  Conflict,
  DrawSettings (..),
  FindConcurrencyConfig (..), FindConflictConfig (..),
  PetriConflict (Conflict, conflictTrans),
  PetriLike,
  PickConcurrencyConfig (..), PickConflictConfig (..),
  placeNames,
  shuffleNames,
  transitionNames,
  traversePetriLike,
  )

import Control.Arrow                    (Arrow (second))
import Control.Monad.Random (
  RandT,
  RandomGen,
  StdGen,
  evalRandT,
  mkStdGen
  )
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Control.Monad.Trans              (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT, runExceptT)
import Data.Bitraversable               (bimapM)
import Data.List                        (nub)
import Data.Map                         (Map)
import Data.Maybe                       (isJust, isNothing)
import Data.String.Interpolate          (i)
import Diagrams.Backend.SVG             (renderSVG)
import Diagrams.Prelude                 (mkWidth)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance, Object, getSingle, lookupSig, unscoped
  )
import System.Random.Shuffle            (shuffleM)
import Text.Read                        (readMaybe)

data FindInstance a = FindInstance {
  drawFindWith :: DrawSettings,
  transitionPair :: a,
  net :: PetriLike String,
  numberOfPlaces :: Int
  }
  deriving (Generic, Read, Show)

data PickInstance = PickInstance {
  drawPickWith :: DrawSettings,
  nets :: Map Int (Bool, PetriLike String)
  }
  deriving (Generic, Read, Show)

findConcurrencyTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance (Concurrent String)
  -> LangM m
findConcurrencyTask path task = do
  pn <- renderWith path "concurrent" (net task) (drawFindWith task)
  paragraph $ text "Considering this Petri net"
  image pn
  paragraph $ text "Which pair of transitions are concurrently activated under the initial marking?"
  paragraph $ do
    text "Please state your answer by giving a pair of concurrently activated transitions. "
    text "Stating as answer: "
    code [i|("t1", "t2")|]
    text " would indicate that transitions t1 and t2 are concurrently activated under the initial marking."
    text " The order of transitions within the pair does not matter here."

findConcurrencyEvaluation
  :: OutputMonad m
  => FindInstance (Concurrent String)
  -> (String, String)
  -> LangM m
findConcurrencyEvaluation task =
  transitionPairEvaluation "are concurrent" (numberOfPlaces task) (ft, st)
  where
    Concurrent (ft, st) = transitionPair task

transitionPairEvaluation
  :: OutputMonad m
  => String
  -> Int
  -> (String, String)
  -> (String, String)
  -> LangM m
transitionPairEvaluation what n (ft, st) is = do
  paragraph $ text "Remarks on your solution:"
  assertion (isTransition fi)
    $ fi ++ " is a valid transition of the given Petri net?"
  assertion (isTransition si)
    $ si ++ " is a valid transition of the given Petri net?"
  assertion (ft == fi && st == si || ft == si && st == fi)
    $ "Given transitions " ++ what ++ "?"
  where
    (fi, si) = is
    isTransition xs
      | 't':xs' <- xs
      , Just x  <- readMaybe xs'
      = x >= 1 && x <= n
      | otherwise
      = False

findConflictTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance Conflict
  -> LangM m
findConflictTask path task = do
  pn <- renderWith path "conflict" (net task) (drawFindWith task)
  paragraph $ text "Considering this Petri net"
  image pn
  paragraph $ text
    "Which pair of transitions are in conflict under the initial marking?"
  paragraph $ do
    text "Please state your answer by giving a pair of conflicting transitions. "
    text "Stating as answer: "
    code [i|("t1", "t2")|]
    text " would indicate that transitions t1 and t2 are in conflict under the initial marking."
    text " The order of transitions within the pair does not matter here. "

findConflictEvaluation
  :: OutputMonad m
  => FindInstance Conflict
  -> (String, String)
  -> LangM m
findConflictEvaluation task =
  transitionPairEvaluation "have a conflict" (numberOfPlaces task) (ft, st)
  where
    (ft, st) = conflictTrans $ transitionPair task

pickConcurrencyTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> PickInstance
  -> LangM m
pickConcurrencyTask path task = do
  paragraph $ text
    "Which of the following Petri nets has exactly one pair of transitions that are concurrently activated?"
  files <- renderPick path "concurrent" task
  images show snd files
  paragraph $ text
    [i|Please state your answer by giving only the number of the Petri net having these concurrently activated transitions.|]
  let plural = wrongInstances task > 1
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that Petri net 1 has exactly two transitions that are concurrently activated (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]

wrongInstances :: PickInstance -> Int
wrongInstances inst = length [False | (False, _) <- M.elems (nets inst)]

pickEvaluation
  :: OutputMonad m
  => PickInstance
  -> Int
  -> LangM m
pickEvaluation = singleChoice "petri nets" . head . M.keys . M.filter fst . nets

pickConflictTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> PickInstance
  -> LangM m
pickConflictTask path task = do
  paragraph $ text
    "Which of the following Petri nets has exactly one pair of transitions that are in conflict?"
  files <- renderPick path "conflict" task
  images show snd files
  paragraph $ text
    [i|Please state your answer by giving only the number of the Petri net having these transitions in conflict.|]
  let plural = wrongInstances task > 1
  paragraph $ do
    text [i|Stating |]
    code "1"
    text [i| as answer would indicate that Petri net 1 has exactly two transitions that are in conflict (and the other Petri #{if plural then "nets don't" else "net doesn't"}!).|]

findConcurrencyGenerate
  :: FindConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance (Concurrent String))
findConcurrencyGenerate config segment seed = do
  (d, c) <- evalRandT (findConcurrency config segment) $ mkStdGen seed
  return $ FindInstance {
    drawFindWith   = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = graphLayout bc
      },
    transitionPair = c,
    net = d,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConcurrencyConfig)

findConcurrency
  :: RandomGen g
  => FindConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, Concurrent String)
findConcurrency = taskInstance
  findTaskInstance
  petriNetFindConcur
  parseConcurrency
  (\c -> alloyConfig (c :: FindConcurrencyConfig))

findConflictGenerate
  :: FindConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO (FindInstance Conflict)
findConflictGenerate config segment seed = do
  (d, c) <- evalRandT (findConflict config segment) $ mkStdGen seed
  return $ FindInstance {
    drawFindWith = DrawSettings {
      withPlaceNames = not $ hidePlaceNames bc,
      withTransitionNames = not $ hideTransitionNames bc,
      with1Weights = not $ hideWeight1 bc,
      withGraphvizCommand = graphLayout bc
      },
    transitionPair = c,
    net = d,
    numberOfPlaces = places bc
    }
  where
    bc = basicConfig (config :: FindConflictConfig)

findConflict
  :: RandomGen g
  => FindConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) (PetriLike String, Conflict)
findConflict = taskInstance
  findTaskInstance
  petriNetFindConfl
  parseConflict
  (\c -> alloyConfig (c :: FindConflictConfig))

pickConcurrencyGenerate
  :: PickConcurrencyConfig
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConcurrencyGenerate = pickGenerate pickConcurrency bc
  where
    bc config = basicConfig (config :: PickConcurrencyConfig)

pickConflictGenerate
  :: PickConflictConfig
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickConflictGenerate = pickGenerate pickConflict bc
  where
    bc config = basicConfig (config :: PickConflictConfig)

pickGenerate
  :: (c -> Int -> RandT StdGen (ExceptT String IO) [(PetriLike String, Maybe a)])
  -> (c -> BasicConfig)
  -> c
  -> Int
  -> Int
  -> ExceptT String IO PickInstance
pickGenerate pick bc config segment seed = flip evalRandT (mkStdGen seed) $ do
  ns <- pick config segment
  ns'  <- shuffleM ns
  let ts = nub $ concat $ transitionNames . fst <$> ns'
      ps = nub $ concat $ placeNames . fst <$> ns'
  ts' <- shuffleM ts
  ps' <- shuffleM ps
  let mapping = BM.fromList $ zip (ps ++ ts) (ps' ++ ts')
  ns'' <- lift $ bimapM (traversePetriLike (`BM.lookup` mapping)) return `mapM` ns'
  return $ PickInstance {
    drawPickWith = DrawSettings {
      withPlaceNames = not $ hidePlaceNames $ bc config,
      withTransitionNames = not $ hideTransitionNames $ bc config,
      with1Weights = not $ hideWeight1 $ bc config,
      withGraphvizCommand = graphLayout $ bc config
      },
    nets = M.fromList $ zip [1 ..] [(isJust m, n) | (n, m) <- ns'']
    }

renderWith
  :: (MonadIO m, OutputMonad m)
  => String
  -> String
  -> PetriLike String
  -> DrawSettings
  -> LangM' m FilePath
renderWith path task net config = do
  f <- lift $ liftIO $ runExceptT $ do
    let file = path ++ task ++ ".svg"
    dia <- drawNet id net
      (not $ withPlaceNames config)
      (not $ withTransitionNames config)
      (not $ with1Weights config)
      (withGraphvizCommand config)
    liftIO $ renderSVG file (mkWidth 250) dia
    return file
  either
    (const $ refuse (english "drawing diagram failed") >> return "")
    return
    f

renderPick
  :: (MonadIO m, OutputMonad m)
  => String
  -> String
  -> PickInstance
  -> LangM' m (Map Int (Bool, String))
renderPick path task config =
  M.foldrWithKey render (return mempty) $ nets config
  where
    render x (b, net) ns = do
      file <- renderWith path (task ++ '-' : show x) net (drawPickWith config)
      M.insert x (b, file) <$> ns

pickConcurrency
  :: RandomGen g
  => PickConcurrencyConfig
  -> Int
  -> RandT g (ExceptT String IO) [(PetriLike String, Maybe (Concurrent String))]
pickConcurrency = taskInstance
  pickTaskInstance
  petriNetPickConcur
  parseConcurrency
  (\c -> alloyConfig (c :: PickConcurrencyConfig))

pickConflict
  :: RandomGen g
  => PickConflictConfig
  -> Int
  -> RandT g (ExceptT String IO) [(PetriLike String, Maybe Conflict)]
pickConflict = taskInstance
  pickTaskInstance
  petriNetPickConfl
  parseConflict
  (\c -> alloyConfig (c :: PickConflictConfig))

findTaskInstance
  :: (RandomGen g, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> RandT g (ExceptT String IO) (PetriLike String, t String)
findTaskInstance f inst = do
  (pl, t) <- lift $ getNet f inst
  (pl', mapping) <- shuffleNames pl
  t'  <- lift $ (`BM.lookup` mapping) `mapM` t
  return (pl', t')

pickTaskInstance
  :: (MonadTrans m, Traversable t)
  => (AlloyInstance -> Either String (t Object))
  -> AlloyInstance
  -> m (ExceptT String IO) [(PetriLike String, Maybe (t String))]
pickTaskInstance parseF inst = lift $ do
  confl <- second Just <$> getNet parseF inst
  net   <- (,Nothing) <$> getDefaultNet inst
  return [confl,net]

petriNetFindConfl :: FindConflictConfig -> String
petriNetFindConfl FindConflictConfig {
  basicConfig,
  advConfig,
  changeConfig,
  uniqueConflictPlace
  } = petriNetAlloy basicConfig changeConfig (Just uniqueConflictPlace) $ Just advConfig

petriNetPickConfl :: PickConflictConfig -> String
petriNetPickConfl PickConflictConfig {
  basicConfig,
  changeConfig,
  uniqueConflictPlace
  } = petriNetAlloy basicConfig changeConfig (Just uniqueConflictPlace) Nothing

petriNetFindConcur :: FindConcurrencyConfig -> String
petriNetFindConcur FindConcurrencyConfig{
  basicConfig,
  advConfig,
  changeConfig
  } = petriNetAlloy basicConfig changeConfig Nothing $ Just advConfig

petriNetPickConcur :: PickConcurrencyConfig -> String
petriNetPickConcur PickConcurrencyConfig{
  basicConfig,
  changeConfig
  } = petriNetAlloy basicConfig changeConfig Nothing Nothing

{-|
Generate code for PetriNet conflict and concurrency tasks
-}
petriNetAlloy
  :: BasicConfig
  -> ChangeConfig
  -> Maybe (Maybe Bool)
  -- ^ Just for conflict task; Nothing for concurrency task
  -> Maybe AdvConfig
  -- ^ Just for find task; Nothing for pick task
  -> String
petriNetAlloy basicC changeC muniquePlace specific
  = [i|module #{moduleName}

#{modulePetriSignature}
#{maybe "" (const modulePetriAdditions) specific}
#{moduleHelpers}
#{modulePetriConcepts}
#{modulePetriConstraints}

pred #{predicate}[#{place}#{defaultActivTrans}#{activated} : set Transitions, #{t1}, #{t2} : Transitions] {
  \#Places = #{places basicC}
  \#Transitions = #{transitions basicC}
  #{compBasicConstraints activated basicC}
  #{compChange changeC}
  #{maybe "" multiplePlaces muniquePlace}
  #{constraints}
  #{compConstraints}
}

run #{predicate} for exactly #{petriScopeMaxSeq basicC} Nodes, #{petriScopeBitwidth basicC} Int
|]
  where
    activated        = "activatedTrans"
    activatedDefault = "defaultActivTrans"
    compConstraints = maybe
      [i|
  #{connected "defaultGraphIsConnected" $ isConnected basicC}
  #{isolated "defaultNoIsolatedNodes" $ isConnected basicC}
  \##{activatedDefault} >= #{atLeastActive basicC}
  theActivatedDefaultTransitions[#{activatedDefault}]|]
      compAdvConstraints
      specific
    conflict = isJust muniquePlace
    constraints :: String
    constraints
      | conflict  = [i|
  no x,y : givenTransitions, z : givenPlaces | conflictDefault[x,y,z]
  all q : #{p} | conflict[#{t1}, #{t2}, q]
  no q : (Places - #{p}) | conflict[#{t1}, #{t2}, q]
  all u,v : Transitions, q : Places |
    conflict[u,v,q] implies #{t1} + #{t2} = u + v|]
      | otherwise = [i|
  no x,y : givenTransitions | x != y and concurrentDefault[x + y]
  #{t1} != #{t2} and concurrent[#{t1} + #{t2}]
  all u,v : Transitions |
    u != v and concurrent[u + v] implies #{t1} + #{t2} = u + v|]
    defaultActivTrans
      | isNothing specific = [i|#{activatedDefault} : set givenTransitions,|]
      | otherwise          = ""
    moduleName
      | conflict  = "PetriNetConfl"
      | otherwise = "PetriNetConcur"
    multiplePlaces unique
      | unique == Just True
      = [i|one #{p}|]
      | unique == Just False
      = [i|not (one #{p})|]
      | otherwise
      = ""
    p  = places1
    place
      | conflict  = [i|#{p} : some Places,|]
      | otherwise = ""
    predicate
      | conflict  = conflictPredicateName
      | otherwise = concurrencyPredicateName
    t1 = transition1
    t2 = transition2

concurrencyPredicateName :: String
concurrencyPredicateName = "showConcurrency"

conflictPredicateName :: String
conflictPredicateName = "showConflict"

skolemVariable :: String -> String -> String
skolemVariable x y = '$' : x ++ '_' : y

concurrencyTransition1 :: String
concurrencyTransition1 = skolemVariable concurrencyPredicateName transition1

concurrencyTransition2 :: String
concurrencyTransition2 = skolemVariable concurrencyPredicateName transition2

conflictPlaces1 :: String
conflictPlaces1 = skolemVariable conflictPredicateName places1

conflictTransition1 :: String
conflictTransition1 = skolemVariable conflictPredicateName transition1

conflictTransition2 :: String
conflictTransition2 = skolemVariable conflictPredicateName transition2

transition1 :: String
transition1 = "transition1"

transition2 :: String
transition2 = "transition2"

places1 :: String
places1 = "places"

{-|
Parses the conflict Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConflict :: AlloyInstance -> Either String (PetriConflict Object)
parseConflict inst = do
  tc1 <- unscopedSingleSig inst conflictTransition1 ""
  tc2 <- unscopedSingleSig inst conflictTransition2 ""
  pc  <- unscopedSingleSig inst conflictPlaces1 ""
  flip Conflict (Set.toList pc)
    <$> ((,) <$> asSingleton tc1 <*> asSingleton tc2)

{-|
Parses the concurrency Skolem variables for singleton of transitions and returns
both as tuple.
It returns an error message instead if unexpected behaviour occurs.
-}
parseConcurrency :: AlloyInstance -> Either String (Concurrent Object)
parseConcurrency inst = do
  t1 <- unscopedSingleSig inst concurrencyTransition1 ""
  t2 <- unscopedSingleSig inst concurrencyTransition2 ""
  Concurrent <$> ((,) <$> asSingleton t1 <*> asSingleton t2)

unscopedSingleSig :: AlloyInstance -> String -> String -> Either String (Set.Set Object)
unscopedSingleSig inst st nd = do
  sig <- lookupSig (unscoped st) inst
  getSingle nd sig

checkFindConcurrencyConfig :: FindConcurrencyConfig -> Maybe String
checkFindConcurrencyConfig FindConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConcurrencyConfig :: PickConcurrencyConfig -> Maybe String
checkPickConcurrencyConfig PickConcurrencyConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig

checkFindConflictConfig :: FindConflictConfig -> Maybe String
checkFindConflictConfig FindConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForFind basicConfig changeConfig

checkPickConflictConfig :: PickConflictConfig -> Maybe String
checkPickConflictConfig PickConflictConfig {
  basicConfig,
  changeConfig
  }
  = checkConfigForPick basicConfig changeConfig
