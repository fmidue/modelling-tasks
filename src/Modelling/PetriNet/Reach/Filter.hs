{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module for filtering out trivial sequences in Petri net reach tasks.

This module provides functions to detect and filter out "trivial" sequences
that students might accidentally guess correctly or that indicate some structure
in the solution which makes it too simple in some sense, such as:
- Cyclic patterns: [t3, t2, t1, t4, t3, t2, t1, t4]
- Repetitive subsequences: [t4, t4, t4, t4] as prefix/suffix
- Grouped repeats: [t3, t3, t2, t2, t1, t1, t4, t4]
-}
module Modelling.PetriNet.Reach.Filter (
  -- * Pattern detection
  isTrivialSequence,
  isCyclicPattern,
  hasRepetitiveSubsequence,
  hasSpaceballsPrefix,
  hasGroupedRepeats,

  -- * Configuration
  FilterConfig(..),
  defaultFilterConfig,
  noFiltering,
) where

import Data.Data                        (Data)
import Data.List                        (group)
import GHC.Generics                     (Generic)

-- | Configuration for trivial sequence filtering
data FilterConfig = FilterConfig {
  -- | Enable filtering of grouped repeats (e.g., @[t3,t3,t2,t2,t1,t1,t4,t4]@)
  filterGroupedRepeats :: !Bool,
  -- | Minimum length of repetitive subsequence to consider trivial
  -- (e.g., @[t4,t4,t4,t4]@ as prefix/suffix)
  --
  -- 'Nothing' means no filtering of such repetitive subsequences
  minRepetitiveLength :: !(Maybe Int),
  -- | Minimum size of Spaceball PIN pattern (e.g., @[t1,t2,t3,t4,t5]@)
  -- to recognise as trivial prefix
  --
  -- 'Nothing' means no filtering of such Spaceballs PIN patterns
  minSpaceballsLength :: !(Maybe Int),
  -- | Maximum cycle length to check for cyclic patterns
  -- (e.g., @[t3,t2,t1,t4,t3,t2,t1,t4]@)
  --
  -- 'Nothing' means no filtering of such cyclic patterns
  maxCycleLength :: !(Maybe Int)
  } deriving (Data, Eq, Generic, Ord, Read, Show)

noFiltering :: FilterConfig
noFiltering = FilterConfig {
  filterGroupedRepeats = False,
  minRepetitiveLength = Nothing,
  minSpaceballsLength = Nothing,
  maxCycleLength = Nothing
  }

-- | Default filter configuration that enables all filters
defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig {
  filterGroupedRepeats = True,
  minRepetitiveLength = Just 3,
  minSpaceballsLength = Just 4,
  maxCycleLength = Just 4
  }

-- | Check if a sequence is considered trivial according to the given configuration
isTrivialSequence :: (Enum a, Eq a) => FilterConfig -> [a] -> Bool
isTrivialSequence config xs =
  maybe False (`hasSpaceballsPrefix` xs) (minSpaceballsLength config)
  || maybe False (`isCyclicPattern` xs) (maxCycleLength config)
  || maybe False (`hasRepetitiveSubsequence` xs) (minRepetitiveLength config)
  || (filterGroupedRepeats config && hasGroupedRepeats xs)

-- | Check if a sequence begins with a Spaceballs PIN pattern
hasSpaceballsPrefix :: (Enum a, Eq a) => Int -> [a] -> Bool
hasSpaceballsPrefix minLength xs
  | length xs < minLength = False
  | otherwise = take minLength xs == take minLength [head xs ..]

-- | Check if a sequence follows a cyclic pattern (e.g., @[t3,t2,t1,t4,t3,t2,t1,t4]@)
-- The pattern is considered cyclic if it can be represented as `take n (cycle pattern)`
-- where `length pattern <= maxCycleLength` and the sequence has at least 2 complete cycles
isCyclicPattern :: Eq a => Int -> [a] -> Bool
isCyclicPattern m xs = any (isCyclicWith xs) [1..min m (length xs `div` 2)]
  where
    isCyclicWith :: Eq a => [a] -> Int -> Bool
    isCyclicWith seqToCheck cycleLength =
      seqToCheck == take (length seqToCheck) (cycle (take cycleLength seqToCheck))

-- | Check if a sequence has repetitive subsequences as prefix or suffix
-- (e.g., [t4,t4,t4,t4] at the beginning or end)
hasRepetitiveSubsequence :: Eq a => Int -> [a] -> Bool
hasRepetitiveSubsequence minLength xs
  | length xs < minLength = False
  | otherwise = hasRepetitivePrefix minLength xs || hasRepetitiveSuffix minLength xs

-- | Check if sequence starts with repetitive elements
hasRepetitivePrefix :: Eq a => Int -> [a] -> Bool
hasRepetitivePrefix minLength xs = allEqual (take minLength xs)
  where
    allEqual [] = True
    allEqual (y:ys) = all (== y) ys

-- | Check if sequence ends with repetitive elements
hasRepetitiveSuffix :: Eq a => Int -> [a] -> Bool
hasRepetitiveSuffix minLength xs = hasRepetitivePrefix minLength (reverse xs)

-- | Check if a sequence has grouped repeats (e.g., @[t3,t3,t3,t1,t1,t1,t4,t4]@)
-- This means each unique element appears in consecutive groups of the same size > 1
hasGroupedRepeats :: Eq a => [a] -> Bool
hasGroupedRepeats xs =
  length groups >= 2      -- At least 2 different groups
  && all (> 1) groupSizes -- Group sizes are > 1
  where
    groups = group xs
    groupSizes = map length groups
