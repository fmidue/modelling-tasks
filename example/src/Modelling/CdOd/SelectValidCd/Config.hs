-- |

module Modelling.CdOd.SelectValidCd.Config where

import Modelling.CdOd.SelectValidCd (
  SelectValidCdConfig (..),
  )
import Modelling.CdOd.RepairCd (
  AllowedProperties (..),
  )
import Modelling.CdOd.Types (
  ArticleToUse (..),
  ClassConfig (..),
  ObjectProperties (..),
  )

task05 :: SelectValidCdConfig
task05 = SelectValidCdConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = True,
    doubleRelationships = True,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = True,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = False,
    wrongCompositionLimits = False
    },
  articleToUse = DefiniteArticle,
  classConfig = ClassConfig {
    classLimits = (5, 5),
    aggregationLimits = (1, Just 2),
    associationLimits = (2, Just 2),
    compositionLimits = (2, Just 2),
    inheritanceLimits = (1, Just 2),
    relationshipLimits = (7, Just 8)
    },
  maxInstances = Just 4000,
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
  shuffleEachCd = True,
  timeout = Nothing
  }

task06 :: SelectValidCdConfig
task06 = SelectValidCdConfig {
  allowedProperties = AllowedProperties {
    compositionCycles = False,
    doubleRelationships = True,
    inheritanceCycles = False,
    reverseInheritances = False,
    reverseRelationships = False,
    selfInheritances = False,
    selfRelationships = False,
    wrongAssociationLimits = True,
    wrongCompositionLimits = False
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
  printNames = False,
  printNavigations = True,
  printSolution = True,
  shuffleEachCd = True,
  timeout = Nothing
  }
