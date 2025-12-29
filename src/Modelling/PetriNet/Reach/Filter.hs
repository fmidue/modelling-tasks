{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module for filtering sequences in Petri net reach/deadlock tasks.

This module provides functions to filter out sequences and solution sets
based on various criteria that make instances too simple or too complicated:

- Cyclic patterns: [t3, t2, t1, t4, t3, t2, t1, t4]
- Repetitive subsequences: [t4, t4, t4, t4] as prefix/suffix
- Grouped repeats: [t3, t3, t2, t2, t1, t1, t4, t4]
- Too many shortest solutions
- Insufficient transition coverage in solutions
- Insufficient number of transitions absent from all solutions
- Solutions are (not) all permutations of each other

The filtering only happens on/with minimal solution sequences for a task.
-}
module Modelling.PetriNet.Reach.Filter (
  -- * Pattern detection
  isCyclicPatternWithAnyOf,
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
import GHC.Generics                     (Generic)

-- | Configuration for sequence filtering
data FilterConfig = FilterConfig {
  -- | Enable filtering of grouped repeats (e.g., @[t3,t3,t3,t2,t2,t2,t1,t1]@)
  rejectGroupedRepeats :: !Bool,
  -- | Threshold length for repetitive subsequences to reject
  -- (e.g., @[t4,t4,t4,t4]@ as prefix/suffix)
  --
  -- Sequences with repetitive subsequences of at least this length are filtered out.
  -- 'Nothing' means no filtering of such repetitive subsequences.
  repetitiveSubsequenceThreshold :: !(Maybe Int),
  -- | Threshold length for Spaceballs PIN prefix pattern to reject
  -- (e.g., @[t1,t2,t3,t4,t5]@ as prefix)
  --
  -- Sequences with Spaceballs prefix patterns of at least this length are filtered out.
  -- 'Nothing' means no filtering of such Spaceballs PIN prefix patterns.
  spaceballsPrefixThreshold :: !(Maybe Int),
  -- | Forbidden cycle lengths for cyclic patterns to reject
  -- (e.g., @[t3,t2,t1,t4,t3,t2,t1,t4]@; always full cycles checked)
  --
  -- Sequences with cyclic patterns having one of these cycle lengths are filtered out.
  -- The list should be sorted and contain only true divisors of the target sequence length.
  -- An empty list means no rejection filtering of such cyclic patterns.
  forbiddenCycleLengths :: ![Int],
  -- | If nonempty, cycle lengths any of which is required for acceptance
  --
  -- Solution sets where no solution has a cyclic pattern with one of these cycle lengths are filtered out.
  -- The list should be sorted and contain only divisors of the target sequence length.
  -- An empty list means no acceptance requirement for cyclic patterns.
  requireCycleLengthsAny :: ![Int],
  -- | Maximum number of shortest solution sequences in a solution set
  --
  -- Solution sets with more than this many sequences are filtered out.
  -- 'Nothing' means no limit on the number of solution sequences.
  solutionSetLimit :: !(Maybe Int),
  -- | Whether all (shortest) solutions must be permutations of each other
  --
  -- * @True@ means filter out instances where solutions are NOT all permutations
  -- * @False@ means don't care about the permutation property
  requireSolutionsArePermutations :: !Bool,
  -- | Minimum number of transitions required to be absent from all solutions
  --
  -- At least this many transitions from the available transitions
  -- must appear in none of the solution sequences.
  -- A value of @0@ means no filtering based on absent transitions.
  absentTransitionsRequirement :: !Int,
  -- | Minimum transition coverage required for each solution
  --
  -- Each solution must use at least this fraction of available transitions.
  -- For example, @4 % 5@ requires that each solution uses at least 80% of
  -- the available transitions. A value of @0@ means no minimum coverage requirement.
  transitionCoverageRequirement :: !(Ratio Int)
  } deriving (Data, Eq, Generic, Ord, Reader, Read, Show, ToDoc)

noFiltering :: FilterConfig
noFiltering = FilterConfig {
  rejectGroupedRepeats = False,
  repetitiveSubsequenceThreshold = Nothing,
  spaceballsPrefixThreshold = Nothing,
  forbiddenCycleLengths = [],
  requireCycleLengthsAny = [],
  solutionSetLimit = Nothing,
  requireSolutionsArePermutations = False,
  absentTransitionsRequirement = 0,
  transitionCoverageRequirement = 0
  }

-- | Default filter configuration that enables all filters
defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig {
  rejectGroupedRepeats = True,
  repetitiveSubsequenceThreshold = Just 3,
  spaceballsPrefixThreshold = Just 4,
  forbiddenCycleLengths = [2, 3],
  requireCycleLengthsAny = [5],
  solutionSetLimit = Just 15,
  requireSolutionsArePermutations = True,
  absentTransitionsRequirement = 1,
  transitionCoverageRequirement = 4 % 5
  }

-- | Check if a sequence has insufficient transition coverage
hasInsufficientTransitionCoverage :: Ord a => Int -> [a] -> Ratio Int -> Bool
hasInsufficientTransitionCoverage totalTransitions transitionSequence minCoverage
  = let usedCount = length (nubOrd transitionSequence)
    in fromIntegral usedCount < minCoverage * fromIntegral totalTransitions

-- | Check if a sequence begins with a Spaceballs PIN pattern
hasSpaceballsPrefix :: (Enum a, Eq a) => Int -> [a] -> Bool
hasSpaceballsPrefix minLength xs = take minLength xs == take minLength [head xs ..]

-- | Check if a sequence follows a cyclic pattern (e.g., @[t3,t2,t1,t4,t3,t2,t1,t4]@)
-- The pattern is considered cyclic if it can be represented as `take n (cycle pattern)`
-- where `length pattern` is one of the given cycle lengths
isCyclicPatternWithAnyOf :: Eq a => [Int] -> [a] -> Bool
isCyclicPatternWithAnyOf cycleLengths xs = any (isCyclicWith (length xs) xs) cycleLengths
  where
    isCyclicWith :: Eq a => Int -> [a] -> Int -> Bool
    isCyclicWith n seqToCheck cycleLength =
      seqToCheck == take n (cycle (take cycleLength seqToCheck))

-- | Check if a sequence has repetitive subsequences as prefix or suffix
-- (e.g., [t4,t4,t4,t4] at the beginning or end)
hasRepetitiveSubsequence :: Eq a => Int -> [a] -> Bool
hasRepetitiveSubsequence minLength xs =
  allEqual (take minLength xs) || allEqual (take minLength (reverse xs))
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
shouldDiscardSolutions :: (Enum a, Ord a) => FilterConfig -> Int -> [[a]] -> Bool
shouldDiscardSolutions FilterConfig{..} numTransitions solutions =
  maybe False (\n -> notNull (drop n solutions)) solutionSetLimit
  || maybe False ((`any` solutions) . hasSpaceballsPrefix) spaceballsPrefixThreshold
  || notNull forbiddenCycleLengths && any (isCyclicPatternWithAnyOf forbiddenCycleLengths) solutions
  || notNull requireCycleLengthsAny && not (any (isCyclicPatternWithAnyOf requireCycleLengthsAny) solutions)
  || maybe False ((`any` solutions) . hasRepetitiveSubsequence) repetitiveSubsequenceThreshold
  || rejectGroupedRepeats && any hasGroupedRepeats solutions
  || transitionCoverageRequirement > 0 && any (hasInsufficientTransitionCoverage numTransitions `flip` transitionCoverageRequirement) solutions
  || absentTransitionsRequirement > 0 && countAbsentTransitions numTransitions solutions < absentTransitionsRequirement
  || requireSolutionsArePermutations && not (areAllPermutationsOfEachOther solutions)

-- | Count the number of transitions that appear in none of the solutions
countAbsentTransitions :: Ord a => Int -> [[a]] -> Int
countAbsentTransitions totalTransitions solutions =
  let usedTransitions = Set.unions (map Set.fromList solutions)
  in totalTransitions - Set.size usedTransitions

-- | Check if all solutions are permutations of each other
areAllPermutationsOfEachOther :: Ord a => [[a]] -> Bool
areAllPermutationsOfEachOther [] = True
areAllPermutationsOfEachOther (firstSolution:restSolutions) =
  let sortedFirst = sort firstSolution
  in all (\solution -> sort solution == sortedFirst) restSolutions
