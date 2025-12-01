-- |

module Modelling.CdOd.NameCdError.Instance where

import qualified Data.Map                         as M (fromList)

import Modelling.Auxiliary.Output  (ExtraText (..))
import Modelling.Auxiliary.Shuffle.All  (ShuffleInstance (..))
import Modelling.CdOd.NameCdError (
  NameCdErrorInstance (..),
  NameCdErrorTaskTextElement (..),
  Reason (..),
  Relevance (..),
  )

import Modelling.CdOd.Types (
  Annotation (..),
  AnnotatedClassDiagram (..),
  CdDrawSettings (..),
  LimitedLinking (..),
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  )

import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  Language (English, German),
  )
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Paragraph, Special, Translated),
  )
import Data.Map                         (Map)

listToFM :: Ord a => [(a, b)] -> Map a b
listToFM = M.fromList

{-|
points: 0.15
-}
task2024_14 :: ShuffleInstance NameCdErrorInstance
task2024_14 = ShuffleInstance {
  taskInstance = NameCdErrorInstance {
    byName = True,
    classDiagram = AnnotatedClassDiagram {
      annotatedClasses = ["Professor", "FacilityManager", "Person", "Room", "Building", "UniversityCampus"],
      annotatedRelationships = [
        Annotation {
          annotated = Right Composition {
            compositionName = "isPartOf",
            compositionPart = LimitedLinking {
              linking = "Room",
              limits = (2, Nothing)
              },
            compositionWhole = LimitedLinking {
              linking = "Building",
              limits = (1, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 4,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Association {
            associationName = "isResponsibleFor",
            associationFrom = LimitedLinking {
              linking = "FacilityManager",
              limits = (1, Just 1)
              },
            associationTo = LimitedLinking {
              linking = "Building",
              limits = (1, Nothing)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 1,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Association {
            associationName = "hasOfficeIn",
            associationFrom = LimitedLinking {
              linking = "Professor",
              limits = (1, Just 1)
              },
            associationTo = LimitedLinking {
              linking = "Room",
              limits = (1, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = True,
            listingPriority = 2,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Inheritance {
            subClass = "Professor",
            superClass = "Person"
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 5,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Inheritance {
            subClass = "FacilityManager",
            superClass = "Person"
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 6,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Composition {
            compositionName = "isOn",
            compositionPart = LimitedLinking {
              linking = "Building",
              limits = (2, Nothing)
              },
            compositionWhole = LimitedLinking {
              linking = "UniversityCampus",
              limits = (1, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 3,
            referenceUsing = DefiniteArticle
            }
          }
        ]
      },
    cdDrawSettings = CdDrawSettings {
      omittedDefaults = OmittedDefaultMultiplicities {
        aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
        associationOmittedDefaultMultiplicity = Just (0, Nothing),
        compositionWholeOmittedDefaultMultiplicity = Nothing
        },
      printNames = True,
      printNavigations = True
      },
    errorReasons = listToFM [
      ('a', (False, Custom (listToFM [
        (English, "contains a syntax error"),
        (German, "enthält einen Syntaxfehler")
        ]))),
      ('b', (False, Custom (listToFM [
        (English, "does not have any corresponding object diagram"),
        (German, "hat gar kein passendes Objektdiagramm")
        ]))),
      ('c', (False, Custom (listToFM [
        (English, "is fully correctly modelled"),
        (German, "ist vollständig richtig modelliert")
        ]))),
      ('d', (True, Custom (listToFM [
        (English, "violates a requirement of the scenario"),
        (German, "hält eine Vorgabe des Szenarios nicht ein")
        ])))
      ],
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "A student received the following scenario:"),
          (German, "Ein Student hat folgendes Szenario erhalten:")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "A university campus consists of different buildings. Facility managers are responsible for buildings, and each building is cared for by a facility manager. A facility manager is a person. Another kind of persons are the professors, who each have a specific room as own office. A building consists of different rooms, not each of which is a professor's office."),
          (German, "Ein Universitätscampus besteht aus verschiedenen Gebäuden. Hausmeister sind für Gebäude zuständig, und es wird jedes Gebäude von einem Hausmeister betreut. Ein Hausmeister ist eine Person. Eine andere Art von Personen sind die Professoren, die jeweils einen bestimmten Raum als eigenes Büro haben. Ein Gebäude besteht aus verschiedenen Räumen, von denen nicht jeder ein Professorenbüro ist.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "He solved the task of creating a class diagram for this scenario in the following way:"),
          (German, "Die Aufgabe, ein Klassendiagramm für dieses Szenario zu entwerfen, hat er folgendermaßen gelöst:")
          ])
        ],
      Paragraph [Special IncorrectCd],
      Paragraph [
        Translated (listToFM [
          (English, "It contains the following relationships between classes:"),
          (German,"Es enthält die folgenden Beziehungen zwischen Klassen:")
          ])
        ],
      Paragraph [Special RelationshipsList],
      Paragraph [
        Translated (listToFM [
          (English, "Analyse and take a stance on the created class diagram regarding the scenario task of the student! And name all the specific relationships that would need to be changed on occasion."),
          (German, "Analysieren Sie und beziehen Sie Stellung zum entworfenen Klassendiagramm hinsichtlich der Szenario-Aufgabe des Studenten! Und nennen Sie alle konkreten Beziehungen, die gegebenenfalls geändert werden müssten.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "The class diagram ..."),
          (German, "Das Klassendiagramm ...")
          ])
        ],
      Paragraph [Special ReasonsList]
      ],
    addText = NoExtraText
    },
  allowLayoutMangling = False,
  shuffleNames = False,
  shuffleOptions = True
  }

{-|
points: 0.15
-}
task2025_12 :: ShuffleInstance NameCdErrorInstance
task2025_12 = task2024_14
