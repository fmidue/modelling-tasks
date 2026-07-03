-- |

module Modelling.CdOd.DifferentNames.Config where

import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  SolutionDisplay (..),
  )
import Modelling.CdOd.Types (
  ClassConfig (..),
  ObjectConfig (..),
  ObjectProperties (..),
  OmittedDefaultMultiplicities (..),
  )

import Control.OutputCapable.Blocks     (ExtraText (..))
import Data.Ratio                       ((%))

{-|
points: 0.15
average generation time per instance: 0:27min
CPU usage: 350%
-}
task2023_12 :: DifferentNamesConfig
task2023_12 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (6, Just 6)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 4000,
  objectConfig = ObjectConfig {
    linkLimits = (10, Just 10),
    linksPerObjectLimits = (1, Just 4),
    objectLimits = (8, 8)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 1:40min
CPU usage: 350%
-}
task2023_13 :: DifferentNamesConfig
task2023_13 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (11, Just 11),
    linksPerObjectLimits = (1, Just 6),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.25
average generation time per instance: 3:00min
CPU usage: 150%
-}
task2023_25 :: DifferentNamesConfig
task2023_25 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (3, Just 3),
    relationshipLimits = (9, Just 9)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 100,
  objectConfig = ObjectConfig {
    linkLimits = (14, Just 16),
    linksPerObjectLimits = (2, Just 6),
    objectLimits = (8, 10)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 1:10min
CPU usage: 355%
-}
task2024_15 :: DifferentNamesConfig
task2024_15 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (6, Just 6)
    },
  withNonTrivialInheritance = Just False,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (9, Just 9),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (7, 7)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.15
average generation time per instance: 1:17min
CPU usage: 346%
-}
task2024_16 :: DifferentNamesConfig
task2024_16 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (11, Just 11),
    linksPerObjectLimits = (1, Just 6),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just True,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.08
average generation time per instance: 1:07min
CPU usage: 227%
-}
task2024_56 :: DifferentNamesConfig
task2024_56 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (3, Just 3),
    relationshipLimits = (9, Just 9)
    },
  withNonTrivialInheritance = Just False,
  maxInstances = Just 100,
  objectConfig = ObjectConfig {
    linkLimits = (14, Just 16),
    linksPerObjectLimits = (2, Just 6),
    objectLimits = (8, 10)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Nothing,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMapping,
  timeout = Nothing,
  withObviousMapping = Nothing,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_19 :: DifferentNamesConfig
task2025_repeat_19 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (6, Just 6)
    },
  withNonTrivialInheritance = Just False,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (9, Just 9),
    linksPerObjectLimits = (0, Just 4),
    objectLimits = (7, 7)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 1 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just False
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMappingAndReprintOD,
  timeout = Nothing,
  withObviousMapping = Just True,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances: 100
maximum concurrent amount of tasks: 20
average generation time per instance on the cluster (without considering concurrency): 2:28min
total run time on the cluster (not including queuing time): 16:14min
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_14 :: DifferentNamesConfig
task2025_14 = DifferentNamesConfig {
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 1),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  withNonTrivialInheritance = Just True,
  maxInstances = Just 10000,
  objectConfig = ObjectConfig {
    linkLimits = (11, Just 11),
    linksPerObjectLimits = (1, Just 6),
    objectLimits = (6, 6)
    },
  objectProperties = ObjectProperties {
    anonymousObjectProportion = 0 % 1,
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  omittedDefaultMultiplicities = OmittedDefaultMultiplicities {
    aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
    associationOmittedDefaultMultiplicity = Just (0, Nothing),
    compositionWholeOmittedDefaultMultiplicity = Nothing
    },
  printSolution = ShowMappingAndReprintOD,
  timeout = Nothing,
  withObviousMapping = Just False,
  extraText = NoExtraText
  }

{-|
points: 0.15
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_20 :: DifferentNamesConfig
task2025_repeat_20 = task2025_14

{-|
points: 0.15
variant 1: concepts are printed in class diagrams
share same instances as task2025_14
average concept generation time per instance (no concurrency): 10~15 mins
used LLM model for generation: gpt-5
approximate input tokens: 3.575 M (1.25 $ / 1M tokens)
approximate output tokens: 4.437 M (10 $ / 1M tokens)
approximate cost: 48.84 $
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_15 :: DifferentNamesConfig
task2025_15 = task2025_14 {
  objectProperties = (objectProperties task2025_14) {
    anonymousObjectProportion = 1 % 1
    }
  }

{-|
points: 0.15
variant 1: concepts are printed in class diagrams
share same instances as task2025_repeat_20
average concept generation time per instance (no concurrency):
used LLM model for generation:
approximate input tokens:
approximate output tokens:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_15 :: DifferentNamesConfig
task2025_repeat_15 = task2025_repeat_20 {
  objectProperties = (objectProperties task2025_repeat_20) {
    anonymousObjectProportion = 1 % 1
    }
  }

{-|
points: 0.15
variant 2: concepts are printed in object diagrams
share same instances as task2025_14
share same concept injection as task2025_15
used as: DifferentNamesRadiobuttonsUnAvailable-Quiz (at the time)
-}
task2025_16 :: DifferentNamesConfig
task2025_16 = task2025_15

{-|
points: 0.15
variant 2: concepts are printed in object diagrams
share same instances as task2025_repeat_20
share same concept injection as task2025_repeat_15
used as: DifferentNamesFormInputRadioButtonsUnAvailable-Quiz
-}
task2025_repeat_16 :: DifferentNamesConfig
task2025_repeat_16 = task2025_repeat_15

{-|
points: 0.15
variant 3: Give scenario descriptions instead of class diagrams with object diagrams
share same instances as task2025_14
share same concept injection as task2025_15
used LLM for story generation: gpt-4o-mini
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_21 :: DifferentNamesConfig
task2025_21 = task2025_15

{-|
points: 0.15
variant 3: Give scenario descriptions instead of class diagrams with object diagrams
share same instances as task2025_repeat_20
share same concept injection as task2025_repeat_15
used LLM for story generation:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_17 :: DifferentNamesConfig
task2025_repeat_17 = task2025_repeat_15

{-|
points: 0.15
variant 4: Only give object diagrams
share same instances as task2025_14
share same concept injection as task2025_15
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_22 :: DifferentNamesConfig
task2025_22 = task2025_15

{-|
points: 0.15
variant 4: Only give object diagrams
share same instances as task2025_repeat_20
share same concept injection as task2025_repeat_15
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_18 :: DifferentNamesConfig
task2025_repeat_18 = task2025_repeat_15

{-|
points: 0.1
the amount of generated instances: 100
maximum concurrent amount of tasks: 100
average generation time per instance on the cluster (without considering concurrency): 2:23min
total run time on the cluster (not including queuing time): 7:34min
average CPU usage: 168.75%
average memory usage: 2785.06 MB
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_55 :: DifferentNamesConfig
task2025_55 = task2025_14

{-|
points: 0.1
the amount of generated instances:
maximum concurrent amount of tasks:
average generation time per instance on the cluster (without considering concurrency):
total run time on the cluster (not including queuing time):
average CPU usage:
average memory usage:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_21 :: DifferentNamesConfig
task2025_repeat_21 = task2025_14

{-
points: 0.1
share same instances as task2025_21
used as: DifferentNamesCheckboxesUnAvailable-Quiz (at the time)
-}
task2025_57 :: DifferentNamesConfig
task2025_57 = task2025_21

{-
points: 0.1
variant 3: Give scenario descriptions instead of class diagrams with object diagrams
share same instances as task2025_repeat_20, but new concept injection
average concept generation time per instance (no concurrency):
used LLM model for generation:
approximate input tokens:
approximate output tokens:
used LLM for story generation:
used as: DifferentNamesFormInputDropdownsUnAvailable-Quiz
-}
task2025_repeat_22 :: DifferentNamesConfig
task2025_repeat_22 = task2025_21
