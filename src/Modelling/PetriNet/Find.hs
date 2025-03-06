{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Modelling.PetriNet.Find (
  FindInstance (..),
  checkFindBasicConfig,
  checkConfigForFind,
  findInitialList,
  findInitialTuple,
  findTaskInstance,
  lToFind,
  toFindEvaluationList,
  toFindEvaluationTuple,
  toFindSyntax,
  ) where

import qualified Data.Bimap                       as BM (lookup)

import Modelling.Auxiliary.Common       (Object)
import Modelling.Auxiliary.Output (
  addPretext,
  )
import Modelling.PetriNet.Diagram (
  getNet,
  )
import Modelling.PetriNet.Reach.Type (
  ShowTransition (ShowTransition),
  Transition (Transition),
  )
import Modelling.PetriNet.Types (
  BasicConfig (..),
  ChangeConfig (..),
  DrawSettings (..),
  GraphConfig (..),
  Net (..),
  checkBasicConfig,
  checkChangeConfig,
  shuffleNames,
  transitionListShow,
  transitionPairShow,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Lens                     (makeLensesFor)
import Control.Monad.Catch              (MonadThrow)
import Control.OutputCapable.Blocks (
  LangM',
  Language (English, German),
  OutputCapable,
  continueOrAbort,
  english,
  german,
  localise,
  translate,
  )
import Control.Monad.Random (
  RandT,
  RandomGen,
  )
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.List                        (sort)
import Data.Map                         (Map)
import Language.Alloy.Call (
  AlloyInstance,
  )
import GHC.Generics                     (Generic)

data FindInstance n a = FindInstance {
  drawFindWith :: !DrawSettings,
  toFind :: !a,
  net :: !n,
  numberOfPlaces :: !Int,
  numberOfTransitions :: !Int,
  showSolution :: !Bool
  }
  deriving (Functor, Generic, Read, Show)

makeLensesFor [("toFind", "lToFind")] ''FindInstance

findInitialTuple :: (Transition, Transition)
findInitialTuple = (Transition 0, Transition 1)

findInitialList :: [Transition]
findInitialList = [Transition 0, Transition 1]

toFindSyntax
  :: OutputCapable m
  => Bool
  -> Int
  -> (Transition, Transition)
  -> LangM' m ()
toFindSyntax withSol n (fi, si) = addPretext $ do
  assertTransition fi
  assertTransition si
  pure ()
  where
    assert = continueOrAbort withSol
    assertTransition t = assert (isValidTransition t) $ translate $ do
      let t' = show $ ShowTransition t
      english $ t' ++ " is a transition of the given Petri net?"
      german $ t' ++ " ist eine Transition des gegebenen Petrinetzes?"
    isValidTransition (Transition x) = x >= 1 && x <= n

findTaskInstance
  :: (MonadThrow m, Net p n, RandomGen g, Traversable t)
  => (AlloyInstance -> m (t Object))
  -> AlloyInstance
  -> RandT g m (p n String, t String)
findTaskInstance f inst = do
  (pl, t) <- lift $ getNet f inst
  (pl', mapping) <- shuffleNames pl
  t'  <- lift $ (`BM.lookup` mapping) `mapM` t
  return (pl', t')

toFindEvaluationTuple
  :: (Num a, OutputCapable m)
  => Map Language String
  -> Bool
  -> (Transition, Transition)
  -> (Transition, Transition)
  -> LangM' m (Maybe String, a)
toFindEvaluationTuple what withSol (ft, st) (fi, si) = do
  let correct = ft == fi && st == si || ft == si && st == fi
      points = if correct then 1 else 0
      maybeSolutionString =
        if withSol
        then Just $ show $ transitionPairShow (ft, st)
        else Nothing
  assert correct $ translate $ do
    english $ "The given transitions " ++ localise English what ++ "?"
    german $ "Die angegebenen Transitionen " ++ localise German what ++ "?"
  pure (maybeSolutionString, points)
  where
    assert = continueOrAbort withSol

toFindEvaluationList
  :: (Num a, OutputCapable m)
  => Map Language String
  -> Bool
  -> [Transition]
  -> [Transition]
  -> LangM' m (Maybe String, a)
toFindEvaluationList what withSol correctTransitions inputTransitions = do
  let correct = sortCorrect correctTransitions == sortCorrect inputTransitions
      points = if correct then 1 else 0
      maybeSolutionString =
        if withSol
        then Just $ show $ transitionListShow correctTransitions
        else Nothing
  assert correct $ translate $ do
    english $ "The given transitions " ++ localise English what ++ "?"
    german $ "Die angegebenen Transitionen " ++ localise German what ++ "?"
  pure (maybeSolutionString, points)
  where
    assert = continueOrAbort withSol
    sortCorrect :: Ord a => [a] -> [a]
    sortCorrect = sort

checkFindBasicConfig :: BasicConfig -> Maybe String
checkFindBasicConfig BasicConfig { atLeastActive }
 | atLeastActive < 2
  = Just "The parameter 'atLeastActive' must be at least 2 to create the task."
 | otherwise = Nothing

checkConfigForFind :: BasicConfig -> ChangeConfig -> GraphConfig -> Maybe String
checkConfigForFind basic change graph =
  checkFindBasicConfig basic
  <|> prohibitHideTransitionNames graph
  <|> checkBasicConfig basic
  <|> checkChangeConfig basic change

prohibitHideTransitionNames :: GraphConfig -> Maybe String
prohibitHideTransitionNames gc
  | hideTransitionNames gc
  = Just "Transition names are required for this task type"
  | otherwise
  = Nothing
