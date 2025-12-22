{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
#if !MIN_VERSION_base(4,18,0)
{-# LANGUAGE DerivingStrategies #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Reach.hs
-}
module Modelling.PetriNet.Reach.Reach (
  -- * Types
  ReachInstance(..),
  NetGoal(..),
  ReachConfig(..),
  NetGoalConfig(..),
  checkReachConfig,

  -- * Generation
  generateReach,

  -- * Solutions
  netGoalSolution,
  netGoalAllSolutions,

  -- * Task creation
  reachTask,
  verifyReach,

  -- * Evaluation
  reachEvaluation,
  reachSyntax,
  reachInitial,

  -- * Configuration
  defaultReachConfig,
  defaultReachInstance,

  -- * Utilities
  bimapReachInstance,
  bimapNetGoal,
  toShowReachInstance,
  toShowNetGoal,
  assertReachPoints,
  isNoLonger,
  reportReachFor,
  transitionsValid,
  levelsWithAlternatives,
  formatSolutionsFeedback,
) where

import qualified Control.Monad.Trans              as Monad (lift)
import qualified Data.Set                         as S (fromList, member, toList, union, empty)

import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Data.Data                        (Data)
import Modelling.Auxiliary.Output (
  hoveringInformation,
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile, isPetriDrawable)
import Modelling.PetriNet.Reach.Filter (
  FilterConfig (..),
  areSolutionsTrivial,
  defaultFilterConfig,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (executes, levels', successors)
import Modelling.PetriNet.Reach.Type (
  Capacity (Unbounded),
  Net (start, transitions),
  Place (..),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  State,
  Transition (..),
  TransitionsList (TransitionsList),
  bimapNet,
  example,
  hasIsolatedNodes,
  mapState,
  mark,
  )

import Control.Applicative              (Alternative, (<|>))
import Control.Functor.Trans            (FunctorTrans (lift))
import Control.Monad                    (forM, guard, msum, when, unless)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (findM, whenJust)
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkFilterConfigWith,
  checkMaxPrintedSolutions,
  )
import Control.OutputCapable.Blocks (
  ArticleToUse (IndefiniteArticle),
  GenericOutputCapable (assertion, code, image, indent, paragraph, text),
  LangM,
  MinimumThreshold (MinimumThreshold),
  OutputCapable,
  Rated,
  collapsed,
  english,
  german,
  printSolutionAndAssertWithMinimum,
  translate,
  translations,
  yesNo,
  )
import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Control.Monad.Random             (MonadRandom, mkStdGen)
import Control.Monad.Trans.Random       (evalRandT)
import Data.Bifunctor                   (Bifunctor (second), bimap)
import Data.Either.Combinators          (whenRight)
import Data.Foldable                    (sequenceA_, traverse_)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (find, singleton, sortBy)
import Data.List.Extra                  (groupSort, nubSort)
import Data.Tuple.Extra                 (fst3)
import Data.Maybe                       (fromMaybe)
import Data.Ord                         (comparing)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
#if !MIN_VERSION_base(4,18,0)
import Data.Typeable                    (Typeable)
#endif
import GHC.Generics                     (Generic)

verifyReach :: (Ord a, Ord t, OutputCapable m, Show a, Show t)
  => ReachInstance a t
  -> LangM m
verifyReach inst = do
  let n = petriNet (netGoal inst)
  validate Default n
  validate Default $ n { start = goal (netGoal inst) }
  assertion (showGoalNet inst || showPlaceNames inst) $ translate $ do
    english "At least one of goal net or place names must be shown."
    german "Mindestens eines von Zielnetz oder Plätze-Namen muss angezeigt werden."
  pure ()

reachTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    Ord s,
    Ord t,
    OutputCapable m,
    Show s,
    Show t
    )
  => Bool
  -> FilePath
  -> ReachInstance s t
  -> LangM m
reachTask showInputHelp path inst = do
  if showGoalNet inst
    then Left
    <$> lift (drawFileWithSettings (n { start = goal (netGoal inst) }))
    else pure (Right $ show $ goal (netGoal inst))
  $>>= \g ->
    lift (drawFileWithSettings n)
  $>>= \img -> reportReachFor
    showInputHelp
    img
    (noLongerThan inst)
    (withLengthHint inst)
    (minLength inst)
    (withMinLengthHint inst)
    (Just g)
  where
    n = petriNet (netGoal inst)
    drawFileWithSettings = drawToFile (not $ showPlaceNames inst) path (drawUsing (netGoal inst))

reportReachFor
  :: OutputCapable m
  => Bool
  -> FilePath
  -> Maybe Int
  -> Maybe Int
  -> Int
  -> Bool
  -> Maybe (Either FilePath String)
  -> LangM m
reportReachFor showInputHelp img noLonger lengthHint minLength showMinLengthHint maybeGoal = do
  paragraph $ translate $ do
    english "For the Petri net"
    german "Gesucht ist für das Petrinetz"
  image img
  paragraph $ case maybeGoal of
    Nothing -> translate $ do
      english "a transition sequence is sought which leads to a marking without successors (i.e., to a deadlock)."
      german "eine Transitionsfolge, die zu einer Markierung ohne Nachfolger (also zu einem Deadlock) führt."
    Just g -> do
      translate $ do
        english "a transition sequence is sought which leads to the following marking:"
        german "eine Transitionsfolge, durch welche die folgende Markierung erreicht wird:"
      paragraph $ either image text g
      pure ()

  when showInputHelp $ do
   paragraph $ translate $ do
      english "State your answer as a sequence of the following kind:"
      german "Geben Sie Ihre Lösung als Auflistung der folgenden Art an:"
   let
      (t1, t2, t3) = (Transition 1, Transition 2, Transition 3)
      showT = show . ShowTransition
      (st1, st2, st3) = (showT t1, showT t2, showT t3)
   code $ show $ TransitionsList [t1, t2, t3]
   paragraph $ translate $ do
    english $ concat [
      "Where giving these three steps means that after firing ",
      st1, ", then ", st2, ", and finally ", st3,
      " (in exactly this order), the sought marking is reached."
      ]
    german $ concat [
      "Wobei die Angabe dieser drei Schritte bedeuten soll, dass nach dem Schalten von ",
      st1, ", danach ", st2, ", und schließlich ", st3,
      " (in genau dieser Reihenfolge), die gesuchte Markierung erreicht wird."
      ]
   pure ()

  paragraph $ case noLonger of
    Nothing ->
      translate $ do
        english "Your answer can be arbitrarily short or long."
        german "Ihre Lösung kann beliebig kurz oder lang sein."

    Just maxL ->
      let
        isExactMatch = showMinLengthHint && maxL == minLength
        (englishConstraint, germanConstraint) =
          if isExactMatch
          then ("have exactly", "muss genau")
          else ("not exceed", "darf maximal")
      in translate $ do
        english $ concat [
          "Your answer must ",
          englishConstraint, " ", show maxL, " steps."]
        german $ concat [
          "Ihre Lösung ", germanConstraint, " ", show maxL,
          "Schritte enthalten."]

  let maxStepsHint = case lengthHint of
        Just maxSteps | showMinLengthHint && maxSteps == minLength -> singleton $ paragraph $ translate $ do
          english [i|The shortest solutions have exactly #{maxSteps} steps.|]
          german [i|Die kürzesten Lösungen haben genau #{maxSteps} Schritte.|]
        Just maxSteps -> singleton $ paragraph $ translate $ do
          english [i|There is a solution with not more than #{maxSteps} steps.|]
          german [i|Es gibt eine Lösung mit nicht mehr als #{maxSteps} Schritten.|]
        Nothing -> []
      minStepsHint = if showMinLengthHint && lengthHint /= Just minLength
        then singleton $ paragraph $ translate $ do
          english [i|There is no solution with less than #{minLength} steps.|]
          german [i|Es gibt keine Lösung mit weniger als #{minLength} Schritten.|]
        else []
      hints = maxStepsHint ++ minStepsHint
      titleText = if length hints > 1
        then translations $ do
          english "Hints on solution length"
          german "Hinweise zur Lösungslänge"
        else translations $ do
          english "Hint on solution length"
          german "Hinweis zur Lösungslänge"
  unless (null hints) $ collapsed True titleText $ sequenceA_ hints
  hoveringInformation True
  pure ()

reachInitial :: ReachInstance s Transition -> TransitionsList
reachInitial = TransitionsList . reverse . S.toList . transitions . petriNet . netGoal

reachSyntax
  :: OutputCapable m
  => ReachInstance s Transition
  -> [Transition]
  -> LangM m
reachSyntax inst ts =
  do transitionsValid (petriNet (netGoal inst)) ts
     isNoLonger (noLongerThan inst) ts
     pure ()

transitionsValid :: OutputCapable m => Net s Transition -> [Transition] -> LangM m
transitionsValid n =
  traverse_ assertTransition . nubSort
  where
    assertTransition t = assertion (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a transition of the given Petri net?"
      german $ t' ++ " ist eine Transition des gegebenen Petrinetzes?"
    isValidTransition =  (`elem` transitions n)

formatSolutionsFeedback
  :: Int
  -> Either [Transition] [[Transition]]
  -> Maybe String
formatSolutionsFeedback maxDisplayValue solutionsList
  | maxDisplayValue <= 0 = Nothing
  | otherwise = case solutionsList of
      Left singleSolution ->
        Just $ show $ TransitionsList singleSolution
      Right (firstSolution : restSolutions) ->
        let displayedSolutions = firstSolution : take (maxDisplayValue - 1) restSolutions
            solutionsText = unlines $ map (show . TransitionsList) displayedSolutions
        in Just $ solutionsText ++
          if length restSolutions < maxDisplayValue
            then "\n(These are all solutions.)"
            else "\n(These are possible solutions, but more exist.)"
      Right [] -> error "formatSolutionsFeedback: solutions should never contain an empty list"

reachEvaluation
  :: (
    Alternative m,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> ReachInstance Place Transition
  -> [Transition]
  -> Rated m
reachEvaluation path reach ts =
  do paragraph $ translate $ do
       english "Start marking:"
       german "Startmarkierung:"
     indent $ text $ show (start n)
     pure ()
  $>> executes path (drawUsing (netGoal reachInstance)) n (map ShowTransition ts)
  $>>= \eitherOutcome -> whenRight eitherOutcome (\outcome ->
    yesNo (outcome == goal (netGoal reachInstance)) $ translate $ do
      english "Reached target marking?"
      german "Zielmarkierung erreicht?"
    )
  $>> assertReachPoints
    aSolution
    ((==) . goal . netGoal)
    minLength
    reachInstance
    ts
    eitherOutcome
  where
    reachInstance = toShowReachInstance reach
    n = petriNet (netGoal reachInstance)
    aSolution = formatSolutionsFeedback (maxDisplayedSolutions reach) (solutions reach)

netGoalSolution :: Ord s => NetGoal s t -> [t]
netGoalSolution netGoal = reverse $ snd $ head $ concatMap
  (filter $ (== goal netGoal) . fst)
  $ levels' $ petriNet netGoal

{-|
Get all possible shortest solutions for a 'NetGoal'

Note: This function does not terminate
if the goal is not reachable and the net is not bounded.
-}
netGoalAllSolutions :: Ord s => NetGoal s t -> [[t]]
netGoalAllSolutions netGoal =
  let goalState = goal netGoal
  in map reverse . maybe [] snd $ find ((== goalState) . fst)
     $ concat $ levelsWithAlternatives $ petriNet netGoal

{-|
Find all shortest paths to all reachable markings
segmented by the length of paths starting with 0.

Each returned trace for a state is in reversed order.
-}
levelsWithAlternatives :: Ord s => Net s t -> [[(State s, [[t]])]]
levelsWithAlternatives n =
  let f _    [] = []
      f done xs =
        let done' = S.union done $ S.fromList $ map fst xs
            next = map (second concat) $ groupSort [ (y, map (t:) ps) |
                (x,ps) <- xs,
                (t,y) <- successors n x,
                not $ S.member y done'
              ]
         in xs : f done' next
  in f S.empty [(start n, [[]])]

assertReachPoints
  :: OutputCapable m
  => Maybe String
  -> (i -> a -> Bool)
  -> (i -> Int)
  -> i
  -> [b]
  -> Either Int a
  -> Rated m
assertReachPoints aCorrectSolution p size inst ts eitherOutcome = do
  let points = either
        partly
        (\x -> if p inst x then 1 else partly $ length ts)
        eitherOutcome
  printSolutionAndAssertWithMinimum
    (MinimumThreshold $ 1 % 3)
    False
    ((IndefiniteArticle,) <$> aCorrectSolution)
    points
  where
    partly x = partiallyCorrect x $ size inst
    partiallyCorrect x y = min 0.6 $
      if y == 0
      then 0
      else toInteger x % toInteger y

isNoLonger :: OutputCapable m => Maybe Int -> [a] -> LangM m
isNoLonger maybeMaxLength ts =
  whenJust maybeMaxLength $ \maxLength ->
    assertion (length ts <= maxLength) $ translate $ do
      english $ unwords [
        "At most",
        show maxLength,
        "steps provided?"
        ]
      german $ unwords [
        "Höchstens",
        show maxLength,
        "Schritte angegeben?"
        ]

data ReachInstance s t = ReachInstance {
  netGoal               :: NetGoal s t,
  minLength             :: Int,
  noLongerThan          :: Maybe Int,
  showGoalNet           :: Bool,
  showPlaceNames        :: Bool,
  maxDisplayedSolutions :: Int,
  solutions             :: Either [t] [[t]],
  withLengthHint        :: Maybe Int,
  withMinLengthHint     :: Bool
  }
  deriving (Generic, Read, Show, Data)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

data NetGoal s t = NetGoal {
  drawUsing         :: GraphvizCommand,
  petriNet          :: Net s t,
  goal              :: State s
  }
  deriving (Generic, Read, Show, Data)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

bimapReachInstance
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> ReachInstance s t
  -> ReachInstance a b
bimapReachInstance f g ReachInstance {..} = ReachInstance {
    netGoal           = bimapNetGoal f g netGoal,
    minLength         = minLength,
    noLongerThan      = noLongerThan,
    showGoalNet       = showGoalNet,
    showPlaceNames    = showPlaceNames,
    maxDisplayedSolutions = maxDisplayedSolutions,
    solutions         = bimap (map g) (map (map g)) solutions,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint
    }

bimapNetGoal
  :: (Ord a, Ord b)
  => (s -> a)
  -> (t -> b)
  -> NetGoal s t
  -> NetGoal a b
bimapNetGoal f g NetGoal {..} = NetGoal {
    drawUsing = drawUsing,
    goal      = mapState f goal,
    petriNet  = bimapNet f g petriNet
    }

toShowReachInstance
  :: ReachInstance Place Transition
  -> ReachInstance ShowPlace ShowTransition
toShowReachInstance = bimapReachInstance ShowPlace ShowTransition

toShowNetGoal
  :: NetGoal Place Transition
  -> NetGoal ShowPlace ShowTransition
toShowNetGoal = bimapNetGoal ShowPlace ShowTransition

data ReachConfig = ReachConfig {
  netGoalConfig         :: NetGoalConfig,
  maxPrintedSolutions   :: Int,
  rejectLongerThan      :: Maybe Int,
  showLengthHint        :: Bool,
  showMinLengthHint     :: Bool,
  showTargetNet         :: Bool,
  showPlaceNamesInNet   :: Bool,
  filterConfig          :: FilterConfig
  }
  deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

data NetGoalConfig = NetGoalConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  drawCommands        :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  postconditionsRange :: (Int, Maybe Int),
  preconditionsRange  :: (Int, Maybe Int)
  }
  deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

defaultReachConfig :: ReachConfig
defaultReachConfig = ReachConfig {
  netGoalConfig = NetGoalConfig {
    numPlaces           = 4,
    numTransitions      = 4,
    Modelling.PetriNet.Reach.Reach.capacity = Unbounded,
    drawCommands        = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
    maxTransitionLength = 6,
    minTransitionLength = 6,
    postconditionsRange = (0, Nothing),
    preconditionsRange  = (0, Nothing)
    },
  maxPrintedSolutions = 0,
  rejectLongerThan    = Just 6,
  showLengthHint      = False,
  showMinLengthHint   = True,
  showTargetNet       = True,
  showPlaceNamesInNet = False,
  filterConfig        = defaultFilterConfig { maxCycleLength = Just 3 }
  }

defaultReachInstance :: ReachInstance Place Transition
defaultReachInstance = ReachInstance {
  netGoal = NetGoal {
    drawUsing         = Circo,
    petriNet          = fst example,
    goal              = snd example
    },
  minLength         = 12,
  noLongerThan      = Nothing,
  showGoalNet       = True,
  showPlaceNames    = False,
  maxDisplayedSolutions = 0,
  solutions         = Left [], -- TO DO: add a solution
  withLengthHint    = Just 12,
  withMinLengthHint = False
}

possibleNetGoals
  :: MonadRandom m
  => NetGoalConfig
  -> m [(Net Place Transition, State Place, [Transition])]
possibleNetGoals NetGoalConfig {..} =
  let ps = [Place 1 .. Place numPlaces]
      tries = forM [1 :: Int .. 1000] $ const $ do
        n <- netLimits vLow vHigh nLow nHigh
            ps
            ts
            capacity
        return $ do
          -- Filter out nets with isolated nodes
          guard $ not $ hasIsolatedNodes n
          (l,zs) <-
            take (maxTransitionLength + 1) $ zip [0 :: Int ..] $ levels' n
          (z', transitions) <- zs
          let d = sum $ do
                p <- ps
                return $ abs (mark (start n) p - mark z' p)
              solutionSequence = reverse transitions
          return ((negate l, d), (n, z', solutionSequence))
      out = do
        xs <- sortBy (comparing fst)
          . concat
          . drop (minTransitionLength + 1)
          <$> tries
        if null xs
          then out
          else pure xs
  in map snd <$> out
  where
    fixMaximum = second (min numPlaces . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum preconditionsRange
    (nLow, nHigh) = fixMaximum postconditionsRange
    ts = [Transition 1 .. Transition numTransitions]

-- | Generate NetGoal with filtering for trivial solutions
generateNetGoal
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => FilterConfig
  -> NetGoalConfig
  -> Int
  -> m (NetGoal Place Transition, Either [Transition] [[Transition]])
generateNetGoal filterConfig config@NetGoalConfig {..} seed =
  evalRandT generate $ mkStdGen seed
  where
    checkNetGoal pn = do
      cmd <- MaybeT $ findM (Monad.lift . isPetriDrawable (fst3 pn)) drawCommands
      let (petri, state, singleSolution) = pn
          netGoal = NetGoal {
            drawUsing   = cmd,
            goal        = state,
            petriNet    = petri
          }
          allShortestSolutions = netGoalAllSolutions netGoal
          availableTransitions = transitions petri
      guard (not $ areSolutionsTrivial filterConfig availableTransitions allShortestSolutions)
      let solutionsList =
            if filterConfig == noFiltering
              then Left singleSolution
              else Right allShortestSolutions
      pure (netGoal, solutionsList)
    generate = do
      xs <- possibleNetGoals config
      maybeNetGoal <- runMaybeT $ msum $ map checkNetGoal xs
      maybe generate pure maybeNetGoal

checkReachConfig :: ReachConfig -> Maybe String
checkReachConfig ReachConfig {..} =
  checkBasicPetriConfig
    (numPlaces netGoalConfig)
    (numTransitions netGoalConfig)
    (capacity netGoalConfig)
    (minTransitionLength netGoalConfig)
    (maxTransitionLength netGoalConfig)
    (preconditionsRange netGoalConfig)
    (postconditionsRange netGoalConfig)
    (drawCommands netGoalConfig)
    rejectLongerThan
    showLengthHint
  <|>
  checkFilterConfigWith
    rejectLongerThan
    (minTransitionLength netGoalConfig)
    (maxTransitionLength netGoalConfig)
    filterConfig
  <|>
  checkMaxPrintedSolutions maxPrintedSolutions filterConfig
  <|>
  if showTargetNet || showPlaceNamesInNet
      then Nothing
      else Just "At least one of showTargetNet or showPlaceNamesInNet must be True"

generateReach
  :: (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => ReachConfig
  -> Int
  -> m (ReachInstance Place Transition)
generateReach ReachConfig {..} seed = do
  (netGoal, solutionsList) <- generateNetGoal filterConfig netGoalConfig seed
  pure $ ReachInstance {
    netGoal           = netGoal,
    minLength         = minTransitionLength netGoalConfig,
    noLongerThan      = rejectLongerThan,
    showGoalNet       = showTargetNet,
    showPlaceNames    = showPlaceNamesInNet,
    solutions         = solutionsList,
    maxDisplayedSolutions = maxPrintedSolutions,
    withLengthHint    =
      if showLengthHint then Just $ maxTransitionLength netGoalConfig else Nothing,
    withMinLengthHint = showMinLengthHint
    }
