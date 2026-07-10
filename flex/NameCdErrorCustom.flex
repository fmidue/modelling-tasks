taskName: NameCdErrorFlex

=============================================

module Global where

import FlexTask.Generic.Form (MultipleChoiceSelection, SingleChoiceSelection)
import Modelling.CdOd.NameCdError

type Submission = (SingleChoiceSelection, MultipleChoiceSelection)
type DescData = (Int,Int,Int)
type FormData = (NameCdErrorInstance, Maybe Char)
type TaskData = NameCdErrorInstance

=============================================

module TaskSettings where

import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  ExtraText(..),
  LangM,
  Language(..),
  OutputCapable
  )
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Paragraph, Special, Translated),
  )
import Modelling.CdOd.NameCdError
import Modelling.CdOd.Types
import Modelling.Auxiliary.Shuffle.All  (ShuffleInstance (..))
import Data.Map                         (Map)
import qualified Data.Map               as M


listToFM :: Ord a => [(a, b)] -> Map a b
listToFM = M.fromList

task :: ShuffleInstance NameCdErrorInstance
task = ShuffleInstance {
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
          (German, "Ein Studierender hat folgendes Szenario erhalten:")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          ( English
          , "A university campus consists of different buildings. " ++
            "Facility managers are responsible for buildings and each building is cared for by a facility manager. " ++
            "A facility manager is a person. Another kind of persons are the professors, who each have a specific room as own office. " ++
            "A building consists of different rooms, not each of which is a professor's office."
          ),
          ( German
          , "Ein Universitätscampus besteht aus verschiedenen Gebäuden. " ++
            "Hausmeister sind für Gebäude zuständig und jedes Gebäude wird von einem Hausmeister betreut. " ++
            "Ein Hausmeister ist eine Person. Eine andere Art von Personen sind die Professoren, die jeweils einen bestimmten Raum als eigenes Büro haben. " ++
            "Ein Gebäude besteht aus verschiedenen Räumen, von denen nicht jeder ein Professorenbüro ist."
          )
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "The student solved the task of creating a class diagram for this scenario in the following way:"),
          (German, "Die Aufgabe, ein Klassendiagramm für dieses Szenario zu erzeugen, hat der Studierende folgendermaßen gelöst:")
          ])
        ],
      Paragraph [Special IncorrectCd],
      Paragraph [
        Translated (listToFM [
          ( English
          , "Analyse and take a stance on the created class diagram regarding the scenario task of the student! " ++
            "If applicable, indicate all the specific relationships that would need to be changed!"
          ),
          ( German
          , "Analysieren Sie und beziehen Sie Stellung zum erzeugten Klassendiagramm hinsichtlich der Szenario-Aufgabe des Studierenden! " ++
            "Gegebenenfalls, führen Sie alle konkreten Beziehungen auf, die geändert werden müssten!"
          )
          ])
        ]
      ],
    addText = NoExtraText
    },
  allowLayoutMangling = False,
  shuffleNames = False,
  shuffleOptions = True
  }

diagramIsCorrectOption :: Maybe Char
diagramIsCorrectOption = Just 'c'

validateSettings :: OutputCapable m => LangM m
validateSettings = pure ()

=============================================

{-# language OverloadedStrings #-}
{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}

module TaskData (getTask) where

import qualified Data.Map               as M
import qualified Data.Text              as T
import Control.Monad.Catch
import Control.Monad.Random             (MonadRandom, evalRandT, getRandom)
import Control.OutputCapable.Blocks     (Language(..))
import Data.Maybe                       (fromMaybe)
import Data.Tuple                       (swap)
import FlexTask.FormUtil                (addCssClass, addJs)
import FlexTask.Generic.Form
import FlexTask.YesodConfig            (Rendered, Widget)
import Modelling.Auxiliary.Shuffle.All (ShuffleInstance(..), shuffleInstance)
import Modelling.CdOd.NameCdError
import Modelling.CdOd.Phrasing          (phraseRelationship)
import Modelling.CdOd.Types
import Data.String.Interpolate (i)
import System.Random                   (mkStdGen)
import Yesod (
  Lang,
  RenderMessage(..),
  SomeMessage(..),
  fieldSettingsLabel,
  julius,
  )

import Global
import TaskSettings


data NameCdErrorReasonLabel = NameCdErrorReasonLabel
data NameCdErrorRelationshipsLabel = NameCdErrorRelationshipsLabel
newtype ReasonLabel = ReasonOption (Char, M.Map Language String)
newtype DueToLabel = DueToOption (Int, M.Map Language String)

instance RenderMessage app NameCdErrorReasonLabel where
  renderMessage _ ("en":_) _ = "Which of these statements about the class diagram is true? (The class diagram ...)"
  renderMessage _ _        _ = "Welche dieser Aussagen zum Klassendiagramm ist zutreffend? (Das Klassendiagramm ...)"

instance RenderMessage app NameCdErrorRelationshipsLabel where
  renderMessage _ ("en":_) _ =
    "In case of a problem, which relationships are involved in it? " <>
    "(choose all that would need to be changed to resolve the problem)"
  renderMessage _ _ _ =
    "Im Fall eines Problems, welche Beziehungen sind darin involviert? " <>
    "(alle auswählen, die zu ändern wären, um das Problem zu beheben)"

instance RenderMessage app ReasonLabel where
  renderMessage _ langs (ReasonOption (letter, m)) =
    T.singleton letter <> ": " <> lookupLang langs m <> "."

instance RenderMessage app DueToLabel where
  renderMessage _ langs (DueToOption (number, m)) =
    T.pack (show number) <> ". " <> lookupLang langs m


lookupLang :: [Lang] -> M.Map Language String -> T.Text
lookupLang lang = T.pack . fromMaybe (error "translation not found") . M.lookup theLang
  where
    theLang = case lang of
      ("de":_) -> German
      ("en":_) -> English
      _        -> error "unsupported language"

getTask :: (MonadRandom m, MonadThrow m) => m (TaskData, String, Rendered Widget)
getTask = do
  seed <- getRandom
  inst <- evalRandT (shuffleInstance task) (mkStdGen seed)
  -- there is no predefined function to look up keys using the value in Data.Map
  let newIsCorrectOption =
        diagramIsCorrectOption >>=
        flip M.lookup (errorReasons $ taskInstance task) >>=
        flip lookup (map swap $ M.toList $ errorReasons inst)
  pure (inst, checkers, form (inst, newIsCorrectOption))

form :: FormData -> Rendered Widget
form (inst@NameCdErrorInstance{..}, mCorrectDiagramOption) =
  let
    reasonToLangs (Custom m) = m
    reasonToLangs _ = error "this task has no predefined reasons"

    letterLangMap = map (fmap (reasonToLangs . snd)) reasonList

    defaults = omittedDefaults cdDrawSettings
    phrase lang article = phraseRelationship lang defaults article Denoted
    phraseRelationship' lang Annotation {..} = phrase
            lang
            (referenceUsing annotation)
            byName
            (printNavigations cdDrawSettings)
            annotated

    relToLangMap rel = M.fromList $ map (\l -> (l, phraseRelationship' l rel)) [German, English]

    relText = map (fmap relToLangMap) $ relevantRelationships inst
  in
    addJs js $ formify (Nothing :: Maybe (SingleChoiceSelection, MultipleChoiceSelection))
      [
        [buttons
          Vertical
          (addCssClass radioClass $ fieldSettingsLabel NameCdErrorReasonLabel)
          $ map (SomeMessage . ReasonOption) letterLangMap
        ],
        [buttons
          Vertical
          (addCssClass checkboxClass $ fieldSettingsLabel NameCdErrorRelationshipsLabel)
          $ map (SomeMessage . DueToOption) relText
        ]
      ]
    where
      radioClass = "flex-radio"
      checkboxClass = "flex-checkbox"
      reasonList = M.toList errorReasons
      errorIndex = flip lookup $ zip (map fst reasonList) [1 :: Integer ..]
      diagramCorrectOption = maybe "" show $ mCorrectDiagramOption >>= errorIndex
      js = [julius|
const formContainers = Array.from(document.getElementsByClassName("flex-form-div"));
const radios = Array.from(document.getElementsByClassName(#{radioClass}));
const checkboxes = Array.from(document.getElementsByClassName(#{checkboxClass}));

const checkboxDisabledRadioValue = #{diagramCorrectOption};

function updateFormValidity() {
  const selectedRadio = radios.find(radio => radio.checked);

  if (selectedRadio?.value === checkboxDisabledRadioValue) {
    checkboxes.forEach(checkbox => {
      checkbox.checked = false;
      checkbox.disabled = true;
      checkbox.setCustomValidity("");
    });

    return;
  }
  else {
    checkboxes.forEach(checkbox => {
      checkbox.disabled = false;
    });

    const message = checkboxes.some(checkbox => checkbox.checked)
      ? ""
      : "Please select at least one relationship.";

    checkboxes.forEach(checkbox => {
      checkbox.setCustomValidity(message);
    });

    return;
  }
}

formContainers.forEach(container => {
  container.addEventListener("change", event => {
    updateFormValidity();
  });
});

document.addEventListener("flex-form:submission-loaded", updateFormValidity);
|]

checkers :: String
checkers = [i|

{-\# language ApplicativeDo \#-}
{-\# language RecordWildCards \#-}
module Check (checkSyntax, checkSemantics) where

import qualified Data.Map               as M
import Capabilities.Cache
import Capabilities.Diagrams
import Capabilities.Graphviz
import Control.Applicative              (Alternative)
import Data.ByteString.UTF8             (toString)
import Data.Either.Extra                (fromEither)
import Data.List.Extra (
  headDef,
  nubOrd,
  replace,
  )
import Data.Tuple.Extra                 (second)
import Data.Yaml                        (encode)
import FlexTask.Generic.Form (
  SingleChoiceSelection,
  getAnswerAsIndex,
  getAnswers,
  )
import Control.OutputCapable.Blocks
import Control.OutputCapable.Blocks.Generic (
  ($>>),
  ($>>=),
  )
import Modelling.Auxiliary.Output
import Modelling.CdOd.NameCdError
import Modelling.CdOd.Types

import Global

getReason :: TaskData -> SingleChoiceSelection -> Char
getReason inst answer = M.keys (errorReasons inst) !! getAnswerAsIndex answer

checkSyntax :: OutputCapable m => TaskData -> Submission -> LangM m
checkSyntax _ _  = pure ()

checkSemantics
  :: (Alternative m, MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> TaskData
  -> Submission
  -> Rated m
checkSemantics _ inst@NameCdErrorInstance{..} (scReason, mcCauses) = addPretext $ do
  let reasonTranslation = M.fromAscList [
        (English, "statement"),
        (German, "Aussage")
        ]
      solutionDueTo = M.fromAscList
        $ map (second (contributingToProblem . annotation))
        relevant
      correctAnswer
        | showSolution = Just (True, DefiniteArticle, replace "reason" "statement" $ toString $ encode $ nameCdErrorSolution inst)
        | otherwise = Nothing
  recoverWith 0 (
    singleChoice reasonTranslation Nothing solutionReason xReason
      $>> multipleChoice
        Nothing
        Nothing
        solutionDueTo
        xDueTo
    )
    $>>= \\points ->
      paragraph (translate $ feedbackOnRelationships points) $>>
      printSolutionAndAssert correctAnswer (fromEither points)
  where
    solutionReason = headDef (error "No correct statement found") . M.keys . M.filter fst $ errorReasons
    relevant = relevantRelationships inst
    chosenRelevant = filter ((`elem` xDueTo) . fst) relevant
    problematicRelationships = filter (contributingToProblem . annotation . snd) relevant
    feedbackOnRelationships points
      -- singleChoice's feedback on the selected reason is enough in these cases
      | null problematicRelationships || null chosenRelevant = pure ()
      | points == Right 1 = do
        english "You correctly gave the relationships constituting the problem."
        german "Sie haben korrekt die das Problem ausmachenden Beziehungen angegeben."
      | problematicRelationships == chosenRelevant = do
        english $
          "You correctly gave the relationships constituting the actual problem, " ++
          "but the selected statement is incorrect."
        german $
          "Sie haben korrekt die das tatsächliche Problem ausmachenden Beziehungen angegeben, " ++
          "aber die ausgewählte Aussage ist nicht korrekt."
      -- skip feedback below if selected reason is incorrect
      | solutionReason /= xReason = pure ()
      -- this guard is never used for this concrete instance with exactly one cause
      -- because the third guard is equivalent then
      | all (`elem` problematicRelationships) chosenRelevant = do
        english $
          "All of the relationships you gave are indeed involved in the problem, " ++
          "but these are not all contributing relationships."
        german $
          "Alle von Ihnen angegebenen Beziehungen sind tatsächlich in das Problem involviert, " ++
          "allerdings sind dies nicht alle beitragenden Beziehungen."
      | all (`elem` chosenRelevant) problematicRelationships = do
        english $
          "You gave all of the relationships that are involved in the problem, " ++
          "but not every relationship you gave does indeed contribute."
        german $
          "Sie haben alle in das Problem involvierten Beziehungen angegeben, " ++
          "allerdings trägt nicht jede von Ihnen angegebene Beziehung tatsächlich bei."
      -- this guard is also overlapped by the previous for instances with one cause
      | any (`elem` problematicRelationships) chosenRelevant = do
        english $
          "You gave part of the relationships that are involved in the problem, " ++
          "but not all the relationships you gave do indeed contribute."
        german $
          "Sie haben einen Teil der in das Problem involvierten Beziehungen angegeben, " ++
          "allerdings tragen nicht alle von Ihnen angegebenen Beziehungen tatsächlich bei."
      | otherwise = do
        english "None of the relationships you gave are actually involved in the problem."
        german "Keine der von Ihnen angegebenen Beziehungen sind tatsächlich in das Problem involviert."

    xDueTo = nubOrd (getAnswers mcCauses)
    xReason = getReason inst scReason
|]

=============================================
{-# Language ApplicativeDo #-}

module Description (description) where

import Capabilities.Cache
import Capabilities.Diagrams
import Capabilities.Graphviz
import Control.OutputCapable.Blocks
import Modelling.CdOd.NameCdError       (nameCdErrorTask)

import Global

description
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, OutputCapable m)
  => FilePath
  -> TaskData
  -> LangM m
description = nameCdErrorTask True False

=============================================

module Parse (parseSubmission) where

import Control.OutputCapable.Blocks (
  LangM',
  ReportT,
  OutputCapable,
  )
import FlexTask.Generic.Parse (
  formParser,
  parseWithOrReport,
  reportWithFieldNumber,
  )

import Global

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission = parseWithOrReport formParser reportWithFieldNumber

