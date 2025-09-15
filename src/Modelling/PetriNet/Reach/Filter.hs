{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module for filtering out trivial sequences in Petri net reach tasks.

This module provides functions to detect and filter out "trivial" sequences
that students might accidentally guess correctly, such as:
- Cyclic patterns: [t1, t2, t3, t4, t1, t2, t3, t4]
- Repetitive subsequences: [t4, t4, t4, t4] as prefix/suffix
- Grouped repeats: [t1, t1, t2, t2, t3, t3, t4, t4]
-}
module Modelling.PetriNet.Reach.Filter (
  -- * Pattern detection
  isTrivialSequence,
  isCyclicPattern,
  hasRepetitiveSubsequence,
  hasGroupedRepeats,

  -- * Filtering
  filterTrivialSolutions,

  -- * Configuration
  FilterConfig(..),
  defaultFilterConfig,
) where

import Data.Data                        (Data)
import Data.List                        (group)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

-- | Configuration for trivial sequence filtering
data FilterConfig = FilterConfig {
  -- | Enable filtering of cyclic patterns (e.g., [t1,t2,t3,t4,t1,t2,t3,t4])
  filterCyclicPatterns :: Bool,
  -- | Enable filtering of repetitive subsequences (e.g., [t4,t4,t4,t4] as prefix/suffix)
  filterRepetitiveSubsequences :: Bool,
  -- | Enable filtering of grouped repeats (e.g., [t1,t1,t2,t2,t3,t3,t4,t4])
  filterGroupedRepeats :: Bool,
  -- | Minimum length of repetitive subsequence to consider trivial (default: 3)
  minRepetitiveLength :: Int,
  -- | Maximum cycle length to check for patterns (default: 4)
  maxCycleLength :: Int
  } deriving (Eq, Generic, Ord, Read, Show, Typeable, Data)

-- | Default filter configuration that enables all filters
defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig {
  filterCyclicPatterns = True,
  filterRepetitiveSubsequences = True,
  filterGroupedRepeats = True,
  minRepetitiveLength = 3,
  maxCycleLength = 4
  }

-- | Check if a sequence is considered trivial according to the given configuration
isTrivialSequence :: Eq a => FilterConfig -> [a] -> Bool
isTrivialSequence config xs =
  (filterCyclicPatterns config && isCyclicPattern (maxCycleLength config) xs) ||
  (filterRepetitiveSubsequences config && hasRepetitiveSubsequence (minRepetitiveLength config) xs) ||
  (filterGroupedRepeats config && hasGroupedRepeats xs)

-- | Check if a sequence follows a cyclic pattern (e.g., [t1,t2,t3,t4,t1,t2,t3,t4])
-- The pattern is considered cyclic if it can be represented as `take n (cycle pattern)`
-- where `length pattern <= maxCycleLength` and the sequence has at least 2 complete cycles
isCyclicPattern :: Eq a => Int -> [a] -> Bool
isCyclicPattern maxCycleLen xs
  | length xs < 4 = False  -- Need at least 4 elements for a meaningful cycle
  | otherwise = any (isCyclicWith xs) [1..min maxCycleLen (length xs `div` 2)]
  where
    isCyclicWith :: Eq a => [a] -> Int -> Bool
    isCyclicWith seqToCheck cycleLen
      | cycleLen <= 0 = False
      | length seqToCheck < cycleLen * 2 = False
      | otherwise =
          seqToCheck == take (length seqToCheck) (cycle (take cycleLen seqToCheck))
          && length seqToCheck >= cycleLen * 2

-- | Check if a sequence has repetitive subsequences as prefix or suffix
-- (e.g., [t4,t4,t4,t4] at the beginning or end)
hasRepetitiveSubsequence :: Eq a => Int -> [a] -> Bool
hasRepetitiveSubsequence minLen xs
  | length xs < minLen = False
  | otherwise = hasRepetitivePrefix minLen xs || hasRepetitiveSuffix minLen xs

-- | Check if sequence starts with repetitive elements
hasRepetitivePrefix :: Eq a => Int -> [a] -> Bool
hasRepetitivePrefix minLen xs =
  any (\len ->
    let prefix = take len xs
        firstElem = head xs
    in length prefix >= minLen && all (== firstElem) prefix
  ) [minLen..length xs]

-- | Check if sequence ends with repetitive elements
hasRepetitiveSuffix :: Eq a => Int -> [a] -> Bool
hasRepetitiveSuffix minLen xs = hasRepetitivePrefix minLen (reverse xs)

-- | Check if a sequence has grouped repeats (e.g., [t1,t1,t2,t2,t3,t3,t4,t4])
-- This means each unique element appears in consecutive groups of the same size > 1
hasGroupedRepeats :: Eq a => [a] -> Bool
hasGroupedRepeats xs
  | length xs < 4 = False  -- Need at least 4 elements
  | otherwise =
      let groups = group xs
          groupSizes = map length groups
      in length groups >= 2 &&  -- At least 2 different groups
         all (> 1) groupSizes && -- All groups have size > 1
         allEqual groupSizes     -- All groups have the same size
  where
    allEqual [] = True
    allEqual (y:ys) = all (== y) ys

-- | Filter out solutions that match trivial patterns
filterTrivialSolutions :: Eq a => FilterConfig -> [[a]] -> [[a]]
filterTrivialSolutions config = filter (not . isTrivialSequence config)
