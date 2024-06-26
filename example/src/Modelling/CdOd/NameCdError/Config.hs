-- |

module Modelling.CdOd.NameCdError.Config where

import Modelling.CdOd.NameCdError (
  NameCdErrorConfig (..),
  NumberOfReasons (..),
  Reason (..),
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  )
import Modelling.CdOd.Types (
  ArticlePreference (..),
  ClassConfig (..),
  ObjectProperties (..),
  Property (..),
  )

{-|
points: 0.15
generation time: 0:20min
CPU usage: 200%
-}
task09 :: NameCdErrorConfig
task09 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (1, Just 1),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (7, Just 7)
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printNames = True,
  printNavigations = True,
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True
  }

{-|
points: 0.15
generation time: 0:20min
CPU usage: 200%
-}
task10 :: NameCdErrorConfig
task10 = NameCdErrorConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
    },
  articleToUse = UseDefiniteArticleWherePossible,
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (2, Just 2),
    relationshipLimits = (8, Just 8)
    },
  maxInstances = Just 4000,
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Just False,
    usesEveryRelationshipName = Just True
    },
  possibleReasons = [
    PreDefined CompositionCycles,
    PreDefined DoubleRelationships,
    PreDefined InheritanceCycles,
    PreDefined MultipleInheritances,
    PreDefined ReverseInheritances,
    PreDefined ReverseRelationships,
    PreDefined SelfInheritances,
    PreDefined SelfRelationships,
    PreDefined WrongAssociationLimits,
    PreDefined WrongCompositionLimits
    ],
  printNames = True,
  printNavigations = False,
  printSolution = True,
  reasonsPerInstance = NumberOfReasons {
    customReasons = 0,
    preDefinedInvalid = 6,
    preDefinedValid = 4
    },
  timeout = Nothing,
  useNames = True
  }
