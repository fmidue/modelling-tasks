-- |

module Modelling.CdOd.RepairCd.Config where

import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  RepairCdConfig (..),
  )
import Modelling.CdOd.Types (
  ArticleToUse (..),
  ClassConfig (..),
  ObjectProperties (..),
  )

task07 :: RepairCdConfig
task07 = RepairCdConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = False,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = True,
    wrongAssociationLimits = False,
    wrongCompositionLimits = True
    },
  articleToUse = DefiniteArticle,
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (2, Just 3),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 1),
    relationshipLimits = (7, Just 8)
    },
  maxInstances = Just 1000,
  objectProperties = ObjectProperties {
    completelyInhabited = Just True,
    hasLimitedIsolatedObjects = True,
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printNames = True,
  printNavigations = False,
  printSolution = True,
  timeout = Nothing,
  useNames = True
  }

task08 :: RepairCdConfig
task08 = RepairCdConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = False,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = True,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = DefiniteArticle,
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
    hasSelfLoops = Nothing,
    usesEveryRelationshipName = Just True
    },
  printExtendedFeedback = True,
  printNames = False,
  printNavigations = True,
  printSolution = True,
  timeout = Nothing,
  useNames = False
  }
