-- |

module Modelling.CdOd.RepairCd.Config where

import Modelling.CdOd.RepairCd (
  RepairCdConfig (..),
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  ArticlePreference (..),
  ClassConfig (..),
  CdConstraints (..),
  CdDrawSettings (..),
  CdMutation (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  RelationshipMutation (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.Ratio                       ((%))

{-|
points: 0.15
average generation time per instance: 5:00h
CPU usage: 600%
-}
task2023_07 :: RepairCdConfig
task2023_07 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = True
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 3),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 8)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 1000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
attention: invalid configuration! (increase maxInstances!)
-}
task2023_08 :: RepairCdConfig
task2023_08 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    RemoveRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship ChangeLimit,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Just True
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = True
    },
  maxInstances = Just 1000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = False,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 4:37:39h
CPU usage: 361%
-}
task2024_12 :: RepairCdConfig
task2024_12 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (5, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just True, -- set to 'Nothing' in future
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 03:54min
CPU usage: 247%
-}
task2024_13 :: RepairCdConfig
task2024_13 = RepairCdConfig {
  allowedCdMutations = [
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Just False
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 3),
    associationLimits = (1, Just 2),
    compositionLimits = (2, Just 3),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Nothing,
      associationOmittedDefaultMultiplicity = Nothing,
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = False,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = False,
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 8:05:42h
CPU usage: 267%
-}
task2024_55 :: RepairCdConfig
task2024_55 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (5, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 1:48:30h
total run time on the cluster (not including queuing time): 3:12:47h
average CPU usage: 212.11%
average memory usage: 1935.99 MB
used as: RepairCdFormInputCheckboxesUnAvailable-Quiz
-}
task2025_repeat_12 :: RepairCdConfig
task2025_repeat_12 = RepairCdConfig {
  allowedCdMutations = [
    AddRelationship,
    MutateRelationship ChangeKind,
    MutateRelationship Flip
    ],
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (1, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (5, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
    printNavigations = False
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 3,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printSolution = True,
  timeout = Nothing,
  useNames = True,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 53:10min
total run time on the cluster (not including queuing time): 2:42:00h
average CPU usage: 157.80%
average memory usage: 1921.89 MB
used as: RepairCdFormInputCheckboxesUnAvailable-Quiz
-}
task2025_repeat_13 :: RepairCdConfig
task2025_repeat_13 = task2024_13
