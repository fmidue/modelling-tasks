-- |

module Modelling.CdOd.NameCdError.Config where

import Modelling.CdOd.NameCdError (
  NameCdErrorConfig (..),
  NumberOfReasons (..),
  Reason (..),
  )
import Modelling.CdOd.Types (
  AllowedProperties (..),
  ArticlePreference (..),
  ClassConfig (..),
  CdConstraints (..),
  CdDrawSettings (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  Property (..),
  )

import Data.Ratio                       ((%))

{-|
points: 0.15
average generation time per instance: 0:20min
CPU usage: 200%
-}
task2023_09 :: NameCdErrorConfig
task2023_09 = NameCdErrorConfig {
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
    anyCompositionCyclesInvolveInheritances = Just True,
    preventWholePartCycles = False,
    preventIsolatedClasses = False,
    onlyCommonMultiplicities = Nothing
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
    printNames = True,
    printNavigations = True
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True,
  extraTexts = []
  }

{-|
points: 0.15
average generation time per instance: 0:20min
CPU usage: 200%
-}
task2023_10 :: NameCdErrorConfig
task2023_10 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing,
    preventWholePartCycles = False,
    preventIsolatedClasses = False,
    onlyCommonMultiplicities = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (8, Just 8)
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
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True,
  extraTexts = []
  }

{-|
points: 0.15
average generation time per instance: 0:57 min
CPU usage: 171%
-}
task2024_10 :: NameCdErrorConfig
task2024_10 = NameCdErrorConfig {
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
    anyCompositionCyclesInvolveInheritances = Just True,
    preventWholePartCycles = False,
    preventIsolatedClasses = True,
    onlyCommonMultiplicities = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 3),
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
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 4,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True,
  extraTexts = []
  }

{-|
points: 0.15
average generation time per instance: 0:38min
CPU usage: 180%
-}
task2024_11 :: NameCdErrorConfig
task2024_11 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = True,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing,
    preventWholePartCycles = True,
    preventIsolatedClasses = True,
    onlyCommonMultiplicities = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (0, Just 1),
    relationshipLimits = (6, Just 7)
    },
  drawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Nothing,
      associationOmittedDefaultMultiplicity = Nothing,
      compositionWholeOmittedDefaultMultiplicity = Nothing
      },
    printNames = True,
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
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = False,
  extraTexts = []
  }

{-|
points: 0.08
average generation time per instance: 0:26min
CPU usage: 208%
-}
task2024_54 :: NameCdErrorConfig
task2024_54 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    invalidInheritanceLimits = True,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  cdConstraints = CdConstraints {
    anyCompositionCyclesInvolveInheritances = Nothing,
    preventWholePartCycles = False,
    preventIsolatedClasses = False,
    onlyCommonMultiplicities = Nothing
    },
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (0, Just 1),
    relationshipLimits = (6, Just 7)
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
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = False,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined InvalidInheritanceLimits,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True,
  extraTexts = []
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:21min
total run time on the cluster (not including queuing time): 2:28min
average CPU usage: 105.05%
average memory usage: 1300.12 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2025_repeat_04 :: NameCdErrorConfig
task2025_repeat_04 = task2024_10

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 1:11min
total run time on the cluster (not including queuing time): 1:14min
average CPU usage: 107.07%
average memory usage: 1337.27 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2025_repeat_05 :: NameCdErrorConfig
task2025_repeat_05 = task2024_11

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:21min
total run time on the cluster (not including queuing time): 2:28min
average CPU usage: 104.72%
average memory usage: 1300.33 MB
used as: NameCdErrorUnAvailable-Quiz
-}
task2025_54 :: NameCdErrorConfig
task2025_54 = task2024_10

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:20min
total run time on the cluster (not including queuing time): 2:25min
average CPU usage: 105.84%
average memory usage: 1303.15 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2025_repeat_06 :: NameCdErrorConfig
task2025_repeat_06 = task2024_10

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 20
average generation time per instance on the cluster (without considering concurrency): 2:24min
total run time on the cluster (not including queuing time): 13:25min
average CPU usage: 106.91%
average memory usage: 1289.42 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2026_04 :: NameCdErrorConfig
task2026_04 = task2024_10

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 20
average generation time per instance on the cluster (without considering concurrency): 1:14min
total run time on the cluster (not including queuing time): 6:40min
average CPU usage: 109.47%
average memory usage: 1220.43 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2026_05 :: NameCdErrorConfig
task2026_05 = task2024_11

{-|
points: 0.2
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
variant 2: semantic variant, finding a specific relationship into an invalid cycle
the number of finally assigned instances: 77
average generation time per instance on the cluster (without considering concurrency): 1:26min
total run time on the cluster (not including queuing time): 1:33min
average CPU usage: 107.97%
average memory usage: 1198.23 MB
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2026_06 :: NameCdErrorConfig
task2026_06 = NameCdErrorConfig
    { allowedProperties =
        AllowedProperties
          { compositionCycles = False,
            doubleRelationships = True,
            inheritanceCycles = True,
            invalidInheritanceLimits = False,
            reverseInheritances = False,
            reverseRelationships = False,
            selfInheritances = False,
            selfRelationships = False,
            wrongAssociationLimits = False,
            wrongCompositionLimits = False
          },
      articleToUse = UseDefiniteArticleWherePossible,
      cdConstraints =
        CdConstraints
          { anyCompositionCyclesInvolveInheritances = Nothing,
            preventWholePartCycles = True,
            preventIsolatedClasses = True,
            onlyCommonMultiplicities = Just True
          },
      classConfig =
        ClassConfig
          { classLimits = (5, 5),
            aggregationLimits = (0, Just 1),
            associationLimits = (3, Just 3),
            compositionLimits = (1, Just 1),
            inheritanceLimits = (3, Just 3),
            relationshipLimits = (7, Just 8)
          },
      drawSettings =
        CdDrawSettings
          { omittedDefaults =
              OmittedDefaultMultiplicities
                { aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
                  associationOmittedDefaultMultiplicity = Just (0, Nothing),
                  compositionWholeOmittedDefaultMultiplicity = Nothing
                },
            printNames = True,
            printNavigations = False
          },
      maxInstances = Just 4000,
      objectProperties =
        ObjectProperties
          { anonymousObjectProportion = 0 % 1,
            completelyInhabited = Just True,
            hasLimitedIsolatedObjects = False,
            hasSelfLoops = Nothing,
            usesEveryRelationshipName = Just True
          },
      possibleReasons =
        [ PreDefined CompositionCycles,
          PreDefined DoubleRelationships,
          PreDefined InheritanceCycles,
          PreDefined InvalidInheritanceLimits,
          PreDefined MultipleInheritances,
          PreDefined ReverseInheritances,
          PreDefined ReverseRelationships,
          PreDefined SelfInheritances,
          PreDefined SelfRelationships,
          PreDefined WrongAssociationLimits,
          PreDefined WrongCompositionLimits
        ],
      printSolution = True,
      reasonsPerInstance =
        NumberOfReasons
          { customReasons = 0,
            preDefinedInvalid = 6,
            preDefinedValid = 4
          },
      timeout = Nothing,
      useNames = True,
      extraTexts = []
    }

{-|
points: 0.2
shared same instances and concept injection as task2026_06 (77/100)
variant 3: scenario variant, a fully correct model matching the scenario description or finding a specific relationship violating the description
the number of finally assigned instances: 34
used LLM for story generation: Qwen3.6-35b-a3b
used as: NameCdErrorFormInputSelectionUnAvailable-Quiz
-}
task2026_07 :: NameCdErrorConfig
task2026_07 = task2026_06
