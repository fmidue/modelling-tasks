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
{-# LANGUAGE ScopedTypeVariables #-}
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
  rejectSpaceballsPattern,
  reportReachFor,
  transitionsValid,
  levelsWithAlternatives,
  provideSolutionsFeedback,
  validateDrawableNetGoal,
) where

import qualified Control.Monad.Trans              as Monad (lift)
import qualified Data.Set                         as S (fromList, member, toList, union, empty)

import Data.List.NonEmpty                 (NonEmpty((:|)), fromList)

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
  shouldDiscardSolutions,
  defaultFilterConfig,
  hasSpaceballsPrefix,
  noFiltering,
  )
import Modelling.PetriNet.Reach.Property (
  Property (Default),
  validate,
  )
import Modelling.PetriNet.Reach.Roll    (netLimits)
import Modelling.PetriNet.Reach.Step    (executes, successors)
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
import Control.Monad                    (guard, msum, replicateM, when, unless)
import Control.Monad.Catch              (MonadCatch, MonadThrow)
import Control.Monad.Extra              (findM, whenJust)
import Control.Monad.Trans.Maybe        (MaybeT (MaybeT, runMaybeT))
import Modelling.PetriNet.Reach.ConfigValidation (
  checkBasicPetriConfig,
  checkFilterConfigWith,
  )
import Control.OutputCapable.Blocks (
  ArticleToUse (IndefiniteArticle),
  GenericOutputCapable (assertion, code, image, indent, refuse, paragraph, text),
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
import Control.Monad.Random             (mkStdGen)
import Control.Monad.Trans.Random       (RandT, evalRandT)
import System.Random.Shuffle            (shuffleM)
import System.Random.Internal           (StdGen)
import Data.Bifunctor                   (Bifunctor (second), bimap)
import Data.Either.Combinators          (whenRight)
import Data.Foldable                    (sequenceA_, traverse_)
import Data.GraphViz                    (GraphvizCommand (..))
import Data.List                        (singleton, sortBy, transpose)
import Data.List.Extra                  (groupSort, nubSort)
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
     rejectSpaceballsPattern (rejectSpaceballsLength inst) ts
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

provideSolutionsFeedback
  :: Int
  -> Either (NonEmpty [Transition]) (NonEmpty [Transition])
  -> Maybe String
provideSolutionsFeedback maxDisplayedSolutions solutionsList
  | maxDisplayedSolutions <= 0 = Nothing
  | otherwise = Just $ case solutionsList of
      Left (oneSolution :| []) ->
        show (TransitionsList oneSolution) ++
          if 1 < maxDisplayedSolutions
            then "\n\n(This is the one shortest solution.)"
            else "\n\n(This is a shortest solution, but more may exist.)"
      Left (firstSolution :| restSolutions) ->
        let displayedSolutions = firstSolution : restSolutions
            solutionsText = unlines $ map (show . TransitionsList) displayedSolutions
        in solutionsText ++
          if 1 + length restSolutions < maxDisplayedSolutions
            then "\n(These are all the shortest solutions.)"
            else "\n(These are shortest solutions, but more may exist.)"
      Right (theOnlySolution :| []) ->
        show (TransitionsList theOnlySolution) ++
          "\n\n(This is the only solution.)"
      Right (firstSolution :| restSolutions) ->
        let displayedSolutions = firstSolution : take (maxDisplayedSolutions - 1) restSolutions
            solutionsText = unlines $ map (show . TransitionsList) displayedSolutions
        in solutionsText ++
          if length restSolutions < maxDisplayedSolutions
            then "\n(These are all the solutions.)"
            else "\n(These are solutions, but more exist.)"

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
    aSolution = provideSolutionsFeedback (maxDisplayedSolutions reach) (shortestSolutions reach)

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

rejectSpaceballsPattern
  :: (Enum t, Eq t, OutputCapable m, Show t)
  => Maybe Int
  -> [t]
  -> LangM m
rejectSpaceballsPattern maybeRejectSpaceballsLength ts =
  when (maybe False (`hasSpaceballsPrefix` ts) maybeRejectSpaceballsLength) $ do
    let longestSpaceballsPrefix = findLongestSpaceballsPrefix ts
        prefixString = show longestSpaceballsPrefix
    refuse $ paragraph $ translate $ do
      english $ concat [
        "The solution (or its prefix) ",
        prefixString,
        " that you submitted might have made for a good PIN in the Spaceballs movie, but is not correct here."
        ]
      german $ concat [
        "Die Lösung (oder ihr Präfix) ",
        prefixString,
        ", die Sie eingereicht haben, wäre vielleicht eine gute PIN im Spaceballs-Film gewesen, ist hier aber nicht korrekt."
        ]

-- | Find the longest Spaceballs-like prefix in a sequence
-- A Spaceballs prefix is one where elements follow the pattern [x, x+1, x+2, ...]
findLongestSpaceballsPrefix :: (Enum a, Eq a) => [a] -> [a]
findLongestSpaceballsPrefix [] = []
findLongestSpaceballsPrefix (x:xs) =
  x : map snd (takeWhile (uncurry (==)) (zip [succ x ..] xs))

data ReachInstance s t = ReachInstance {
  netGoal           :: NetGoal s t,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  showGoalNet       :: Bool,
  showPlaceNames    :: Bool,
  maxDisplayedSolutions :: Int,
  -- | Solutions to the reach task.
  -- 'Left' contains (some) shortest solutions when no filtering is applied.
  -- 'Right' contains all solutions when filtering is applied.
  -- Note: 'Left' may not contain all shortest solutions, only up to 'maxDisplayedSolutions'.
  shortestSolutions :: Either (NonEmpty [t]) (NonEmpty [t]),
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Bool,
  -- | Minimum length of Spaceballs PIN pattern to reject during syntax checking.
  -- If set to @Just n@, sequences starting with @n@ or more consecutive transitions
  -- (e.g., @[t1, t2, t3, t4]@) will be rejected.
  rejectSpaceballsLength :: Maybe Int
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
    shortestSolutions = bimap (fmap (map g)) (fmap (map g)) shortestSolutions,
    withLengthHint    = withLengthHint,
    withMinLengthHint = withMinLengthHint,
    rejectSpaceballsLength = rejectSpaceballsLength
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
  netGoalConfig       :: NetGoalConfig,
  maxPrintedSolutions :: Int,
  rejectLongerThan    :: Maybe Int,
  showLengthHint      :: Bool,
  showMinLengthHint   :: Bool,
  showTargetNet       :: Bool,
  showPlaceNamesInNet :: Bool,
  filterConfig        :: FilterConfig
  }
  deriving (Generic, Read, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

data NetGoalConfig = NetGoalConfig {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  -- | Draw commands in order of preference
  drawPreferenceOrder :: [GraphvizCommand],
  maxTransitionLength :: Int,
  minTransitionLength :: Int,
  -- | Maximum number of places where token counts may differ between start and goal state.
  -- Must be in the range @1..numPlaces@.
  maxPlacesChanged    :: Int,
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
    numPlaces           = 6,
    numTransitions      = 6,
    Modelling.PetriNet.Reach.Reach.capacity = Unbounded,
    drawPreferenceOrder = [Dot, Neato, TwoPi, Circo, Fdp, Sfdp, Osage, Patchwork],
    maxTransitionLength = 6,
    minTransitionLength = 6,
    maxPlacesChanged    = 3,
    postconditionsRange = (0, Nothing),
    preconditionsRange  = (0, Nothing)
    },
  maxPrintedSolutions = 0,
  rejectLongerThan    = Just 6,
  showLengthHint      = False,
  showMinLengthHint   = True,
  showTargetNet       = True,
  showPlaceNamesInNet = False,
  filterConfig        = defaultFilterConfig { forbiddenCycleLengths = [], requireCycleLengthsAny = [3], transitionCoverageRequirement = 1 % 2 }
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
  shortestSolutions = Left ([] :| []), -- TO DO: add a solution
  withLengthHint    = Just 12,
  withMinLengthHint = False,
  rejectSpaceballsLength = Nothing
}

findNetGoalWithSolutions
  :: forall m. (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => FilterConfig
  -> Int
  -> NetGoalConfig
  -> MaybeT (RandT StdGen m) (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
findNetGoalWithSolutions filterConfig maxPrintedSolutions NetGoalConfig {..} =
  let ps = [Place 1 .. Place numPlaces]
      tries :: RandT StdGen m [[ [(Int, MaybeT (RandT StdGen m) (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition])))] ]]
      tries = replicateM 1000 $ do
        n <- netLimits vLow vHigh nLow nHigh
            ps
            ts
            capacity
        return $ do
         -- Filter out nets with isolated nodes
         guard $ not $ hasIsolatedNodes n
         zs <-
            take (maxTransitionLength - minTransitionLength + 1)
            $ drop minTransitionLength
            $ levelsWithAlternatives n
         return $ do
          (z', transitionSequences) <- zs
          let d = sum placeDifferences
              placeDifferences = do
                p <- ps
                let diff = mark (start n) p - mark z' p
                guard (diff /= 0)
                return (abs diff)
              allShortestSolutions = map reverse transitionSequences
          guard (maxPlacesChanged == numPlaces || maxPlacesChanged >= length placeDifferences)
          return (d, do
            (cmd, solutionsList) <- validateDrawableNetGoal
              n drawPreferenceOrder allShortestSolutions filterConfig numTransitions maxPrintedSolutions
            let netGoal = NetGoal {
                  drawUsing   = cmd,
                  goal        = z',
                  petriNet    = n
                }
            pure (netGoal, solutionsList))
      out :: RandT StdGen m (Maybe (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition])))
      out = do
        xss <- tries
        let grouped = reverse $ transpose xss
            sortByDistance
              :: [[(Int, MaybeT (RandT StdGen m) (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition])))]]
              -> [(Int, MaybeT (RandT StdGen m) (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition])))]
            sortByDistance = sortBy (comparing fst) . concat
            xs = map (msum . map snd . sortByDistance) grouped
        if null xs
          then out
          else runMaybeT (msum xs)
  in MaybeT out
  where
    fixMaximum :: (Int, Maybe Int) -> (Int, Int)
    fixMaximum = second (min numPlaces . fromMaybe maxBound)
    (vLow, vHigh) = fixMaximum preconditionsRange
    (nLow, nHigh) = fixMaximum postconditionsRange
    ts = [Transition 1 .. Transition numTransitions]

-- | Validate drawability and filter criteria, then prepare solutions for output
validateDrawableNetGoal
  :: (Enum t, MonadCatch m, MonadDiagrams m, MonadGraphviz m, Ord p, Ord t, Show p, Show t)
  => Net p t
  -> [GraphvizCommand]
  -> [[t]]
  -> FilterConfig
  -> Int
  -> Int
  -> MaybeT (RandT StdGen m)
       (GraphvizCommand, Either (NonEmpty [t]) (NonEmpty [t]))
validateDrawableNetGoal petri drawCommands allShortestSolutions filterConfig numTransitions maxPrintedSolutions = do
  guard (not $ shouldDiscardSolutions filterConfig numTransitions allShortestSolutions)
  cmd <- MaybeT $ findM (Monad.lift . isPetriDrawable petri) drawCommands
  solutionsList <-
    if filterConfig == noFiltering
      then pure $ Left $ fromList (take (max 1 maxPrintedSolutions) allShortestSolutions)
      else if maxPrintedSolutions >= length allShortestSolutions
        then pure $ Right $ fromList allShortestSolutions
        else Monad.lift $ Right . fromList <$> shuffleM allShortestSolutions
  pure (cmd, solutionsList)

-- | Generate NetGoal with filtering for trivial solutions
generateNetGoal
  :: forall m. (MonadCatch m, MonadDiagrams m, MonadGraphviz m)
  => FilterConfig
  -> Int
  -> NetGoalConfig
  -> Int
  -> m (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
generateNetGoal filterConfig maxPrintedSolutions netGoalConfig seed =
  evalRandT generate $ mkStdGen seed
  where
    generate
      :: RandT StdGen m (NetGoal Place Transition, Either (NonEmpty [Transition]) (NonEmpty [Transition]))
    generate =
      maybe generate pure =<< runMaybeT (findNetGoalWithSolutions filterConfig maxPrintedSolutions netGoalConfig)

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
    (drawPreferenceOrder netGoalConfig)
    rejectLongerThan
    showLengthHint
  <|>
  (let maxPlacesDiff = maxPlacesChanged netGoalConfig
   in if maxPlacesDiff < 1
        then Just "maxPlacesChanged must be at least 1"
        else if maxPlacesDiff > numPlaces netGoalConfig
             then Just "maxPlacesChanged cannot be greater than numPlaces"
             else Nothing)
  <|>
  checkFilterConfigWith
    rejectLongerThan
    (minTransitionLength netGoalConfig)
    (numTransitions netGoalConfig)
    filterConfig
  <|>
  (if maxPrintedSolutions < 0
    then Just "maxPrintedSolutions must be non-negative"
    else case solutionSetLimit filterConfig of
      Just maxSolutions | maxPrintedSolutions > maxSolutions ->
        Just "maxPrintedSolutions cannot be greater than solutionSetLimit"
      _ -> Nothing)
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
  (netGoal, solutionsList) <- generateNetGoal filterConfig maxPrintedSolutions netGoalConfig seed
  pure $ ReachInstance {
    netGoal           = netGoal,
    minLength         = minTransitionLength netGoalConfig,
    noLongerThan      = rejectLongerThan,
    showGoalNet       = showTargetNet,
    showPlaceNames    = showPlaceNamesInNet,
    shortestSolutions = solutionsList,
    maxDisplayedSolutions = maxPrintedSolutions,
    withLengthHint    =
      if showLengthHint then Just $ maxTransitionLength netGoalConfig else Nothing,
    withMinLengthHint = showMinLengthHint,
    rejectSpaceballsLength = spaceballsPrefixThreshold filterConfig
    }
