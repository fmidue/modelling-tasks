-- |

module Modelling.CdOd.MatchCdOd.Config where

import Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  OdDistributionConfig (..),
  )
import Modelling.CdOd.Types (
  CdMutation (..),
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  RelationshipMutation (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.Ratio                       ((%))

{-|
points: 0.15
average generation time per instance: 2:00min
CPU usage: 150%
-}
task2023_14 :: MatchCdOdConfig
task2023_14 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 1),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 1:10min
CPU usage: 150%
-}
task2023_15 :: MatchCdOdConfig
task2023_15 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 4000,
  objectConfig = ObjectConfig {
    linkLimits = (2, Just 6),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (3, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 16:54min
CPU usage: 134%
-}
task2024_17 :: MatchCdOdConfig
task2024_17 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeLimit
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 1),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just False,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 13:14min
CPU usage: 130%
-}
task2024_18 :: MatchCdOdConfig
task2024_18 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship Flip
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (5, Just 6),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (5, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just False
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 5:14min
CPU usage: 127%
-}
task2024_19 :: MatchCdOdConfig
task2024_19 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (7, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 8)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 2:44min
CPU usage: 133%
-}
task2024_20 :: MatchCdOdConfig
task2024_20 = MatchCdOdConfig {
  allowedCdMutations = [
    AddRelationship
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (6, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (5, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 7 % 8,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 5:56min
CPU usage: 137%
-}
task2024_57 :: MatchCdOdConfig
task2024_57 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeLimit
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 1),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (6, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Nothing
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just False,
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 7:13min
CPU usage: 141%
-}
task2024_58 :: MatchCdOdConfig
task2024_58 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship Flip
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (5, Just 6),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (5, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 6:16min
CPU usage: 146%
-}
task2024_59 :: MatchCdOdConfig
task2024_59 = MatchCdOdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind
    ],
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (7, Just 8),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (6, 8)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Nothing
    },
  odDistribution = OdDistributionConfig {
    odCount = 5,
    maxPerJustOneCd = 2,
    maxSharedBetweenBothCds = 2,
    maxNoCd = 2
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = True,
  timeout = Nothing,
  withNonTrivialInheritance = Just False,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 19:18min
total run time on the cluster (not including queuing time): 1:10:30h
average CPU usage: 175.75%
average memory usage: 3777 MB
-}
task2025_17 :: MatchCdOdConfig
task2025_17 = task2024_17

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 8:20min
total run time on the cluster (not including queuing time): 18:21min
average CPU usage: 107.96%
average memory usage: 3792 MB
-}
task2025_18 :: MatchCdOdConfig
task2025_18 = task2024_18

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 12:07min
total run time on the cluster (not including queuing time): 33:51min
average CPU usage: 106.16%
average memory usage: 5232.74 MB
-}
task2025_19 :: MatchCdOdConfig
task2025_19 = task2024_19

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 50
average generation time per instance on the cluster (without considering concurrency): 7:54min
total run time on the cluster (not including queuing time): 19:07min
average CPU usage: 105.74%
average memory usage: 3530.61 MB
-}
task2025_20 :: MatchCdOdConfig
task2025_20 = task2024_20

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 8:07min
total run time on the cluster (not including queuing time): 9:00min
average CPU usage: 105.19%
average memory usage: 3747.66 MB
-}
task2025_56 :: MatchCdOdConfig
task2025_56 = task2025_18
