{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module for filtering sequences in Petri net reach tasks.

This module provides functions to filter out sequences and solution sets
based on various criteria that make instances either too simple or too complicated:

Too simple criteria (making instances trivial):
- Cyclic patterns: [t3, t2, t1, t4, t3, t2, t1, t4]
- Repetitive subsequences: [t4, t4, t4, t4] as prefix/suffix
- Grouped repeats: [t3, t3, t2, t2, t1, t1, t4, t4]
- Too many shortest solutions
- Insufficient transition coverage in solutions

Too complicated criteria (filtering for manageable complexity):
- Solutions are all permutations of each other
- Insufficient number of transitions absent from all solutions
-}
module Modelling.PetriNet.Reach.Filter (
  -- * Pattern detection
  isTrivialSequence,
  isCyclicPattern,
  hasRepetitiveSubsequence,
  hasSpaceballsPrefix,
  hasGroupedRepeats,
  hasInsufficientTransitionCoverage,

  -- * Solution set validation
  shouldDiscardSolutions,

  -- * Configuration
  FilterConfig(..),
  defaultFilterConfig,
  noFiltering,
) where

import qualified Data.Set                         as Set

import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Data.Data                        (Data)
import Data.List                        (group, sort)
import Data.List.Extra                  (notNull, nubOrd)
import Data.Ratio                       (Ratio, (%))
import Data.Set                         (Set)
import GHC.Generics                     (Generic)

-- | Configuration for sequence filtering
data FilterConfig = FilterConfig {
  -- | Enable filtering of grouped repeats (e.g., @[t3,t3,t3,t2,t2,t2,t1,t1]@)
  filterGroupedRepeats :: !Bool,
  -- | Minimum length of repetitive subsequence to consider trivial
  -- (e.g., @[t4,t4,t4,t4]@ as prefix/suffix)
  --
  -- 'Nothing' means no filtering of such repetitive subsequences
  minRepetitiveLength :: !(Maybe Int),
  -- | Minimum size of Spaceballs PIN pattern (e.g., @[t1,t2,t3,t4,t5]@)
  -- to recognise as trivial prefix
  --
  -- 'Nothing' means no filtering of such Spaceballs PIN patterns
  minSpaceballsLength :: !(Maybe Int),
  -- | Maximum cycle length to check for cyclic patterns
  -- (e.g., @[t3,t2,t1,t4,t3,t2,t1,t4]@)
  --
  -- 'Nothing' means no filtering of such cyclic patterns
  maxCycleLength :: !(Maybe Int),
  -- | Maximum number of shortest solutions allowed
  --
  -- 'Nothing' means no limit on the number of solutions
  maxNumberOfSolutions :: !(Maybe Int),
  -- | Minimum fraction of available transitions that must appear in each solution
  --
  -- For example, @4 % 5@ requires that each solution uses at least 80% of
  -- the available transitions. Hence, '0' means no minimum coverage requirement.
  minTransitionCoverage :: !(Ratio Int),
  -- | Minimum number of transitions that must be absent from all minimal solutions
  --
  -- If set to @Just k@, at least @k@ transitions from the available transitions
  -- must appear in none of the minimal solution sequences. This helps ensure
  -- instances are not too complicated by requiring some transitions to be unused.
  --
  -- 'Nothing' means no filtering based on absent transitions
  minAbsentTransitions :: !(Maybe Int),
  -- | Whether all minimal solutions should be permutations of each other
  --
  -- * @Just True@ means filter out instances where solutions are NOT all permutations
  -- * @Just False@ means filter out instances where solutions ARE all permutations
  -- * 'Nothing' means don't care about the permutation property
  solutionsArePermutations :: !(Maybe Bool)
  } deriving (Data, Eq, Generic, Ord, Reader, Read, Show, ToDoc)

noFiltering :: FilterConfig
noFiltering = FilterConfig {
  filterGroupedRepeats = False,
  minRepetitiveLength = Nothing,
  minSpaceballsLength = Nothing,
  maxCycleLength = Nothing,
  maxNumberOfSolutions = Nothing,
  minTransitionCoverage = 0,
  minAbsentTransitions = Nothing,
  solutionsArePermutations = Nothing
  }

-- | Default filter configuration that enables all filters
defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig {
  filterGroupedRepeats = True,
  minRepetitiveLength = Just 3,
  minSpaceballsLength = Just 4,
  maxCycleLength = Just 4,
  maxNumberOfSolutions = Just 15,
  minTransitionCoverage = 4 % 5,
  minAbsentTransitions = Nothing,
  solutionsArePermutations = Nothing
  }

-- | Check if a sequence is considered trivial according to the given configuration
isTrivialSequence :: (Enum a, Ord a) => FilterConfig -> Set a -> [a] -> Bool
isTrivialSequence config availableTransitions xs =
  maybe False (`hasSpaceballsPrefix` xs) (minSpaceballsLength config)
  || maybe False (`isCyclicPattern` xs) (maxCycleLength config)
  || maybe False (`hasRepetitiveSubsequence` xs) (minRepetitiveLength config)
  || (filterGroupedRepeats config && hasGroupedRepeats xs)
  || hasInsufficientTransitionCoverage availableTransitions xs (minTransitionCoverage config)

-- | Check if a sequence has insufficient transition coverage
-- A sequence is considered to have insufficient coverage if it doesn't use enough
-- of the available transitions
hasInsufficientTransitionCoverage :: (Ord a) => Set a -> [a] -> Ratio Int -> Bool
hasInsufficientTransitionCoverage availableTransitions transitionSequence minCoverage
  = let usedCount = length (nubOrd transitionSequence)
        totalTransitions = Set.size availableTransitions
    in fromIntegral usedCount < minCoverage * fromIntegral totalTransitions

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
  | otherwise = allEqual (take minLength xs) || allEqual (take minLength (reverse xs))
  where
    allEqual [] = True
    allEqual (y:ys) = all (== y) ys

-- | Check if a sequence has grouped repeats (e.g., @[t3,t3,t3,t1,t1,t1,t4,t4]@)
-- This means each unique element appears in consecutive groups of size > 1
hasGroupedRepeats :: Eq a => [a] -> Bool
hasGroupedRepeats xs =
  length groups >= 2      -- At least 2 different groups
  && all (> 1) groupSizes -- Group sizes are > 1
  where
    groups = group xs
    groupSizes = map length groups

-- | Check if a set of solutions should be discarded according to the given configuration
--
-- Returns 'True' if the solution set should be discarded (filtered out),
-- 'False' if it should be kept.
--
-- This function filters instances based on multiple criteria:
--
-- * Too simple criteria (making instances trivial):
--
--     - Too many solutions
--     - Individual sequences with trivial patterns (cyclic, repetitive, etc.)
--
-- * Too complicated criteria (filtering for manageable complexity):
--
--     - Insufficient number of transitions absent from all solutions
--     - All solutions are (or are not) permutations of each other
shouldDiscardSolutions :: (Enum a, Ord a) => FilterConfig -> Set a -> [[a]] -> Bool
shouldDiscardSolutions config availableTransitions solutions =
  maybe False (\n -> notNull (drop n solutions)) (maxNumberOfSolutions config)
  || config { maxNumberOfSolutions = Nothing } /= noFiltering && any (isTrivialSequence config availableTransitions) solutions
  || maybe False (\k -> countAbsentTransitions availableTransitions solutions < k) (minAbsentTransitions config)
  || maybe False (\expected -> areAllPermutationsOfEachOther solutions /= expected) (solutionsArePermutations config)

-- | Count the number of transitions that appear in none of the solutions
countAbsentTransitions :: Ord a => Set a -> [[a]] -> Int
countAbsentTransitions availableTransitions solutions =
  let usedTransitions = Set.unions (map Set.fromList solutions)
      absentTransitions = Set.difference availableTransitions usedTransitions
  in Set.size absentTransitions

-- | Check if all solutions are permutations of each other
--
-- Returns 'True' if all solutions are permutations of the same sequence,
-- 'False' otherwise. An empty list or single solution returns 'True'.
areAllPermutationsOfEachOther :: Ord a => [[a]] -> Bool
areAllPermutationsOfEachOther [] = True
areAllPermutationsOfEachOther [_] = True
areAllPermutationsOfEachOther (firstSolution:restSolutions) =
  let sortedFirst = sort firstSolution
  in all (\solution -> sort solution == sortedFirst) restSolutions
