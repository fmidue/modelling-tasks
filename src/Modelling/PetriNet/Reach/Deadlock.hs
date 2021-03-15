{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
originally from Autotool (https://gitlab.imn.htwk-leipzig.de/autotool/all0)
based on revision: ad25a990816a162fdd13941ff889653f22d6ea0a
based on file: collection/src/Petri/Deadlock.hs
-}
module Modelling.PetriNet.Reach.Deadlock where

import qualified Data.Map                         as M (fromList)
import qualified Data.Set                         as S (fromList, toList)

import Modelling.Auxiliary.Output (
  OutputMonad (assertion, image, paragraph, text),
  )
import Modelling.PetriNet.Reach.Draw    (drawToFile)
import Modelling.PetriNet.Reach.Property (Property (Default), validate)
import Modelling.PetriNet.Reach.Roll    (net)
import Modelling.PetriNet.Reach.Step    (deadlocks, execute, successors)
import Modelling.PetriNet.Reach.Type (
  Transition (..),
  Place (..),
  Net (..),
  Capacity (Unbounded, AllBounded),
  State (State),
  )

import Control.Monad                    (foldM, forM, guard)
import Control.Monad.IO.Class           (MonadIO)
import Control.Monad.Random             (MonadRandom, evalRand, mkStdGen)
import Data.List                        (maximumBy)
import Data.Ord                         (comparing)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

data PetriDeadlock = PetriDeadlock
  deriving (Typeable, Generic)

verifyDeadlock
  :: (OutputMonad m, Show a, Show t, Ord t, Ord a)
  => PetriDeadlock
  -> Net a t
  -> m ()
verifyDeadlock PetriDeadlock = validate Default

reportDeadlock
  :: (OutputMonad m, MonadIO m, Ord s, Ord t, Show s, Show t)
  => FilePath
  -> Net s t
  -> m ()
reportDeadlock path n = do
  paragraph $ text "Gesucht ist für das Petri-Netz"
  g <- drawToFile path 0 n
  image g
  paragraph $ text $ unlines [
    "eine Transitionsfolge,",
    "die zu einem Zustand ohne Nachfolger (Deadlock) führt."
    ]

initialDeadlock :: Net s a -> [a]
initialDeadlock n = reverse $ S.toList $ transitions n

totalDeadlock
  :: (OutputMonad m, MonadIO m, Show t, Show s, Ord t, Ord s)
  => Net s t
  -> [t]
  -> m ()
totalDeadlock n ts = do
  out <- foldM
      (\z (k,t) -> do
         paragraph $ text $ "Schritt" ++ show k
         Modelling.PetriNet.Reach.Step.execute k n t z)
      (start n)
      (zip [1 :: Int ..] ts)
  assertion (null $ successors n out) "Zielzustand hat keine Nachfolger?"

data Config = Config {
  numPlaces :: Int,
  numTransitions :: Int,
  capacity :: Capacity Place,
  maxTransitionLength :: Int
  }
  deriving (Typeable, Generic)

example :: Config
example =
  Config {
  numPlaces = 4,
  numTransitions = 4,
  Modelling.PetriNet.Reach.Deadlock.capacity = AllBounded 1,
  maxTransitionLength = 10
  }

generateDeadlock :: Config -> Int -> Net Place Transition
generateDeadlock conf seed = snd $ tries 1000 conf seed

tries :: Int -> Config -> Int -> (Int, Net Place Transition)
tries n conf seed = maximumBy (comparing fst) $ concat out
  where
    randWith x f = evalRand f $ mkStdGen $ seed * n + x
    out = forM [1 .. n] $ \x ->
      randWith x $ Modelling.PetriNet.Reach.Deadlock.try conf

try :: MonadRandom m => Config -> m [(Int, Net Place Transition)]
try conf = do
  let ps = [Place 1 .. Place (numPlaces conf)]
      ts = [Transition 1 .. Transition (numTransitions conf)]
  n <- Modelling.PetriNet.Reach.Roll.net
      ps
      ts
      (Modelling.PetriNet.Reach.Deadlock.capacity conf)
  return $ do
    let (no,yeah) = span (null . snd)
          $ take (maxTransitionLength conf)
          $ zip [0 :: Int ..]
          $ deadlocks n
    guard $ not $ null yeah
    return (length no, n)

expl :: Net Int Int
expl =
  Net {
  places = S.fromList [1, 2, 3, 4, 5],
  transitions = S.fromList [1, 2, 3, 4, 5],
  connections = [
      ([1], 1, [1, 2, 3]),
      ([2], 2, [3, 4]),
      ([3], 3, [4, 5]),
      ([4], 4, [5, 1]),
      ([5], 5, [1, 2]),
      ([1, 2, 3, 4, 5], 7, [])
      ],
    Modelling.PetriNet.Reach.Type.capacity = Unbounded,
    start = State $ M.fromList [(1, 1), (2, 0), (3, 0), (4, 0), (5, 0)]
  }
