taskName: NameCdErrorFlex

=============================================

module Global where

import FlexTask.Generic.Form (MultipleChoiceSelection, SingleChoiceSelection)
import Modelling.CdOd.NameCdError

type Submission = (SingleChoiceSelection, MultipleChoiceSelection)
type DescData = (Int,Int,Int)
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
            "In case, indicate all the specific relationships that would need to be changed!"
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
import FlexTask.Generic.Form
import FlexTask.YesodConfig            (Rendered, Widget)
import Modelling.Auxiliary.Shuffle.All (shuffleInstance)
import Modelling.CdOd.NameCdError
import Modelling.CdOd.Types
import Data.String.Interpolate (i)
import System.Random                   (mkStdGen)
import Yesod (
  Lang,
  RenderMessage(..),
  SomeMessage(..),
  fieldSettingsLabel,
  )

import Global
import TaskSettings
import Phrasing


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
  pure (inst, checkers, form inst)

form :: TaskData -> Rendered Widget
form inst@NameCdErrorInstance{..} =
  let
    reasonToLangs (Custom m) = m
    reasonToLangs _ = error "this task has no predefined reasons"

    reasonList = M.toList errorReasons
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
    formify (Nothing :: Maybe (SingleChoiceSelection, MultipleChoiceSelection))
      [
        [buttons
          Vertical
          (fieldSettingsLabel NameCdErrorReasonLabel)
          $ map (SomeMessage . ReasonOption) letterLangMap
        ],
        [buttons
          Vertical
          (fieldSettingsLabel NameCdErrorRelationshipsLabel)
          $ map (SomeMessage . DueToOption) relText
        ]
      ]

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
import Control.Monad                    (when)
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
    $>>= \\points -> do
      paragraph $ translate $ classDiagramDescription points
      pure ()
    $>> printSolutionAndAssert correctAnswer $ fromEither points
  where
    solutionReason = headDef (error "No correct statement found") . M.keys . M.filter fst $ errorReasons
    relevant = relevantRelationships inst
    chosenRelevant = filter ((`elem` xDueTo) . fst) relevant
    correctRelationships = filter (contributingToProblem . annotation . snd) relevant
    classDiagramDescription points
      | null correctRelationships = descriptionForCorrectDiagram points
      | otherwise = descriptionForFaultyDiagram points

    descriptionForFaultyDiagram points
      | points == Right 1 = do
        english "You correctly gave the relationships constituting the problem."
        german "Sie haben korrekt die das Problem ausmachenden Beziehungen angegeben."
      | null chosenRelevant = when (solutionReason == xReason) $ do
        english "But you did not give any relationships contributing to the problem."
        german "Allerdings haben Sie keine zum Problem beitragenden Beziehungen angegeben."
      | correctRelationships == chosenRelevant = do
        english $
          "You correctly gave the relationships constituting the actual problem, " ++
          "but the selected statement is incorrect."
        german $
          "Sie haben korrekt die das tatsächliche Problem ausmachenden Beziehungen angegeben, " ++
          "aber die ausgewählte Aussage ist nicht korrekt."
      -- this guard is never used for this concrete instance with exactly one cause
      -- because one of the previous two guards would already have matched
      | all (contributingToProblem . annotation . snd) chosenRelevant = do
        english $
          "All of the relationships you gave are indeed involved in the problem, " ++
          "but these are not all contributing relationships."
        german $
          "Alle von Ihnen angegebenen Beziehungen sind tatsächlich in das Problem involviert, " ++
          "allerdings sind dies nicht alle beitragenden Beziehungen."
      | any (contributingToProblem . annotation . snd) chosenRelevant = do
        english $
          "You gave part of the relationships that are involved in the problem, " ++
          "but not all the relationships you gave do indeed contribute."
        german $
          "Sie haben einen Teil der in das Problem involvierten Beziehungen angegeben, " ++
          "allerdings tragen nicht alle von Ihnen angegebenen Beziehungen tatsächlich bei."
      | otherwise = do
        english "None of the relationships you gave are actually involved in the problem."
        german "Keine der von Ihnen angegebenen Beziehungen sind tatsächlich in das Problem involviert."

    -- This feedback would be given if there is no problem, i.e., if the diagram is correct.
    -- (also not used for the current task instance)
    descriptionForCorrectDiagram points
      | points == Right 1 = do
        english "Your submitted solution is correct."
        german "Ihre eingereichte Lösung ist korrekt."
      | null chosenRelevant = do
        english "Your selection indicating that no relationship is involved in a problem is correct."
        german "Ihre Angabe, dass keine Beziehung zu einem Problem beiträgt, ist richtig."
      | otherwise = do
        english "You selected relationships as contributing to the problem, but there are none."
        german "Sie haben Beziehungen als zum Problem beitragend angegebenen, allerdings gibt es keine solchen."

    xReason = getReason inst scReason
    xDueTo = nubOrd (getAnswers mcCauses)
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
description = nameCdErrorTask False

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

=============================================

-- The following are copies of unexposed modules from modelling-tasks

{-# LANGUAGE LambdaCase #-}
module Phrasing (
  phraseChange,
  phraseRelationship,
  trailingCommaGerman,
  ) where

import qualified PhrasingGerman    as German
import qualified PhrasingEnglish   as English

import Control.OutputCapable.Blocks (
  ArticleToUse,
  Language (English, German),
  )

import Modelling.Types (
  Change,
  )
import Modelling.CdOd.Types (
  AnyRelationship,
  OmittedDefaultMultiplicities,
  PhrasingKind,
  )

phraseChange
  :: Language
  -> OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange = \case
  English -> English.phraseChange
  German -> German.phraseChange

phraseRelationship
  :: Language
  -> OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> Bool
  -> Bool
  -> AnyRelationship String String -> String
phraseRelationship = \case
  English -> English.phraseRelationship
  German -> German.phraseRelationship

trailingCommaGerman :: String -> String
trailingCommaGerman = German.trailingComma

=========================

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module PhrasingGerman (
  phraseChange,
  phraseRelationship,
  trailingComma,
  ) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Auxiliary.Util    (oneAndOther)
import PhrasingCommon   (phraseChangeWith, PhrasingStrings (..))
import Modelling.CdOd.Types (
  AnyRelationship,
  DefaultedLimitedLinking (..),
  InvalidRelationship (..),
  LimitedLinking (..),
  NonInheritancePhrasing (..),
  OmittedDefaultMultiplicities (..),
  PhrasingKind (..),
  Relationship (..),
  defaultedLimitedLinking,
  sortLimits,
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.String.Interpolate          (iii)
import Data.Tuple.Extra                 (curry3)

phraseChange
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange = phraseChangeWith germanStrings

germanStrings :: PhrasingStrings
germanStrings = PhrasingStrings
  { changeNothing = "verändere nichts"
  , addPrefix = "ergänze "
  , removePrefix = "entferne "
  , replacePrefix = "ersetze "
  , byInfix = " durch "
  , postProcess = trailingComma
  , phraseRelationWith = phraseRelation
  }

trailingComma :: String -> String
trailingComma xs
      | ',' `elem` xs = xs ++ ","
      | otherwise     = xs

femaleArticle :: ArticleToUse -> String
femaleArticle = \case
  DefiniteArticle -> "die"
  IndefiniteArticle -> "eine"

phraseRelationship
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> Bool
  -> Bool
  -> AnyRelationship String String
  -> String
phraseRelationship defaultMultiplicities article kind byName withDir =
  phraseRelation defaultMultiplicities article kind phrasing
  where
    phrasing = toPhrasing byName withDir

phraseRelation
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> NonInheritancePhrasing
  -> AnyRelationship String String
  -> String
phraseRelation OmittedDefaultMultiplicities {..} article = curry3 $ \case
  (kind,_, Left InvalidInheritance {..}) -> [iii|
    #{femaleArticle article} Vererbung,
    bei der #{linking invalidSubClass} von #{linking invalidSuperClass} erbt
    |]
    ++ phraseParticipations
      kind
      (defaultedInheritance invalidSubClass)
      (defaultedInheritance invalidSuperClass)
  (_, _, Right Inheritance {..}) -> [iii|
    #{femaleArticle article} Vererbung,
    bei der #{subClass} von #{superClass} erbt
    |]
  (_, ByName, Right Association {..}) -> "Assoziation " ++ associationName
  (_, ByName, Right Aggregation {..}) -> "Aggregation " ++ aggregationName
  (_, ByName, Right Composition {..}) -> "Komposition " ++ compositionName
  (kind, how, Right Association {..})
    | from <- defaultedAssociation associationFrom
    , to <- defaultedAssociation associationTo
    -> case (how, kind, linking associationFrom == linking associationTo) of
      (Lengthy, Participations, True) -> [iii|
        #{femaleArticle article} Selbst-Assoziation
        für #{linking associationFrom},
        bei der #{linking associationFrom}
        an einem Ende #{phraseLimitDefault $ defaultedLimits from}
        und am anderen Ende #{phraseLimitDefault $ defaultedLimits to}
        beteiligt ist
        |]
      (Lengthy, Denoted, True)
        | denoted <- uncurry denotions
          $ oneAndOther "einem Ende" "dem anderen Ende"
          $ sortLimits from to
        -> [iii|
          #{femaleArticle article} Selbst-Assoziation
          für #{linking associationFrom}
          |] ++ denoted
      (Lengthy, _, False) -> femaleArticle article ++ " Assoziation"
        ++ phraseParticipations kind from to
      (ByDirection, Participations, True) -> [iii|
        #{femaleArticle article} Selbst-Assoziation
        für #{linking associationFrom},
        bei der #{linking associationFrom}
        am Anfang #{phraseLimitDefault $ defaultedLimits from}
        und am Ende #{phraseLimitDefault $ defaultedLimits to} beteiligt ist
        |]
      (ByDirection, Denoted, True)
        | denoted <- uncurry denotions
          $ uncurry sortLimits
          $ oneAndOther "seinem Anfang" "seinem Pfeilende" (from, to)
        -> [iii|
          #{femaleArticle article} Selbst-Assoziation
          für #{linking associationFrom}
          |] ++ denoted
      (ByDirection, _, False) -> [iii|
        #{femaleArticle article} Assoziation von #{linking associationFrom}
        nach #{linking associationTo}
        |] ++ phraseParticipations kind from to
  (kind, _, Right Aggregation {..})
    | part <- defaultedAssociation aggregationPart
    , whole <- defaultedAssociation aggregationWhole
    ->
      if linking aggregationPart == linking aggregationWhole
      then [iii|
        #{femaleArticle article} Selbst-Aggregation
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{femaleArticle article} Beziehung, die #{linking aggregationWhole}
        eine Aggregation aus #{linking aggregationPart}s macht
        |] ++ phraseParticipations kind whole part
  (kind, _, Right Composition {..})
    | part <- defaultedAssociation compositionPart
    , whole <- defaultedCompositionWhole compositionWhole
    ->
      if linking compositionPart == linking compositionWhole
      then [iii|
        #{femaleArticle article} Selbst-Komposition
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{femaleArticle article} Beziehung, die #{linking compositionWhole}
        eine Komposition aus #{linking compositionPart}s macht
        |] ++ phraseParticipations kind whole part
  where
    defaultedCompositionWhole =
      defaultedLimitedLinking compositionWholeOmittedDefaultMultiplicity
    defaultedAssociation =
      defaultedLimitedLinking associationOmittedDefaultMultiplicity
    defaultedInheritance = defaultedLimitedLinking Nothing

selfParticipatesPartWhole
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
selfParticipatesPartWhole Denoted part whole = [iii|
  für #{defaultedLinking part},
  #{which}
  |]
  where
    which = uncurry denotions $ sortLimits
      part {defaultedLinking = "dem Teil-Ende"}
      whole {defaultedLinking = "dem Ganzen-Ende"}
selfParticipatesPartWhole Participations part whole = [iii|
  für #{defaultedLinking part},
  wobei es #{phraseLimitDefault $ defaultedLimits part} als Teil
  und #{phraseLimitDefault $ defaultedLimits whole} als Ganzes beteiligt ist
  |]

phraseParticipations
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
phraseParticipations = \case
  Denoted -> denotions
  Participations -> participations

denotions
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
denotions one other = case (defaultedRange one, defaultedRange other) of
  (Nothing, Nothing) -> [iii|, bei der keine Multiplizitäten angegeben sind|]
  (Nothing, Just otherRange) -> [iii|
    , bei der keine Multiplizität neben #{defaultedLinking one}
    und #{otherRange} neben #{defaultedLinking other} angegeben ist
    |]
  (Just oneRange, Nothing) -> [iii|
    , bei der keine Multiplizität neben #{defaultedLinking other}
    und #{oneRange} neben #{defaultedLinking one} angegeben ist
    |]
  (Just oneRange, Just otherRange) -> [iii|
    , bei der die Multiplizität
    #{oneRange} neben #{defaultedLinking one}
    und #{otherRange} neben #{defaultedLinking other} angegeben ist
    |]

participations
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
participations one other = [iii|
  , wobei #{defaultedLinking one} #{phraseLimitDefault $ defaultedLimits one}
  und #{defaultedLinking other} #{phraseLimitDefault $ defaultedLimits other}
  beteiligt ist
  |]

phraseLimitDefault :: Maybe (Int, Maybe Int) -> String
phraseLimitDefault = maybe "mit der Standardmultiplizität" phraseLimit

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "gar nicht"
phraseLimit (1, Just 1)  = "genau einmal"
phraseLimit (2, Just 2)  = "genau zweimal"
phraseLimit (-1, Just n) = "*.." ++ show n ++ "-mal"
phraseLimit (m, Nothing) = show m ++ "..*-mal"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ "-mal"

=========================
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module PhrasingEnglish (
  phraseChange,
  phraseRelationship,
  ) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Auxiliary.Util    (oneAndOther)
import PhrasingCommon   (phraseChangeWith, PhrasingStrings (..))
import Modelling.CdOd.Types (
  AnyRelationship,
  DefaultedLimitedLinking (..),
  InvalidRelationship (..),
  LimitedLinking (..),
  NonInheritancePhrasing (..),
  OmittedDefaultMultiplicities (..),
  PhrasingKind (..),
  Relationship (..),
  defaultedLimitedLinking,
  sortLimits,
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.String.Interpolate          (iii)
import Data.Tuple.Extra                 (curry3)

phraseChange
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange = phraseChangeWith englishStrings

englishStrings :: PhrasingStrings
englishStrings = PhrasingStrings
  { changeNothing = "change nothing"
  , addPrefix = "add "
  , removePrefix = "remove "
  , replacePrefix = "replace "
  , byInfix = " by "
  , postProcess = id
  , phraseRelationWith = phraseRelation
  }

consonantArticle :: ArticleToUse -> String
consonantArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "a"

vowelArticle :: ArticleToUse -> String
vowelArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "an"

phraseRelationship
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> Bool
  -> Bool
  -> AnyRelationship String String
  -> String
phraseRelationship defaultMultiplicities article kind byName withDir =
  phraseRelation defaultMultiplicities article kind phrasing
  where
    phrasing = toPhrasing byName withDir

phraseRelation
  :: OmittedDefaultMultiplicities
  -> ArticleToUse
  -> PhrasingKind
  -> NonInheritancePhrasing
  -> AnyRelationship String String
  -> String
phraseRelation OmittedDefaultMultiplicities {..} article = curry3 $ \case
  (kind,_, Left InvalidInheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{linking invalidSubClass} inherits from #{linking invalidSuperClass}
    and #{phraseParticipations
      kind
      (defaultedInheritance invalidSubClass)
      (defaultedInheritance invalidSuperClass)
      }
    |]
  (_, _, Right Inheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{subClass} inherits from #{superClass}
    |]
  (_, ByName, Right Association {..}) -> "association " ++ associationName
  (_, ByName, Right Aggregation {..}) -> "aggregation " ++ aggregationName
  (_, ByName, Right Composition {..}) -> "composition " ++ compositionName
  (kind, how, Right Association {..})
    | from <- defaultedAssociation associationFrom
    , to <- defaultedAssociation associationTo
    -> case (how, kind, linking associationFrom == linking associationTo) of
      (Lengthy, Participations, True)
        | fromIt <- from {defaultedLinking = "it"}
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom}
          where #{participates fromIt} at one end
          and #{phraseLimitDefault $ defaultedLimits to} at the other end
          |]
      (Lengthy, Denoted, True)
        | denoted <- uncurry denotions
          $ oneAndOther "one end" "the other end"
          $ sortLimits from to
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom} #{denoted}
          |]
      (Lengthy, _, False) -> [iii|
        #{vowelArticle article} association
        #{phraseParticipations kind from to}
        |]
      (ByDirection, Participations, True)
        | fromIt <- from {defaultedLinking = "it"}
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom}
          where #{participates fromIt} at its beginning
          and #{phraseLimitDefault $ defaultedLimits to} at its arrow end
          |]
      (ByDirection, Denoted, True)
        | denoted <- uncurry denotions
          $ uncurry sortLimits
          $ oneAndOther "its beginning" "its arrow end" (from, to)
        -> [iii|
          #{consonantArticle article} self-association
          for #{linking associationFrom} #{denoted}
          |]
      (ByDirection, _, False) -> [iii|
        #{vowelArticle article} association from #{linking associationFrom}
        to #{linking associationTo}
        #{phraseParticipations kind from to}
        |]
  (kind, _, Right Aggregation {..})
    | part <- defaultedAssociation aggregationPart
    , whole <- defaultedAssociation aggregationWhole
    ->
      if linking aggregationPart == linking aggregationWhole
      then [iii|
        #{consonantArticle article} self-aggregation
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{consonantArticle article} relationship
        that makes #{linking aggregationWhole}
        an aggregation of #{linking aggregationPart}s
        #{phraseParticipations kind whole part}
        |]
  (kind, _, Right Composition {..})
    | part <- defaultedAssociation compositionPart
    , whole <- defaultedCompositionWhole compositionWhole
    ->
      if linking compositionPart == linking compositionWhole
      then [iii|
        #{consonantArticle article} self-composition
        #{selfParticipatesPartWhole kind part whole}
        |]
      else [iii|
        #{consonantArticle article} relationship
        that makes #{linking compositionWhole}
        a composition of #{linking compositionPart}s
        #{phraseParticipations kind whole part}
        |]
  where
    defaultedCompositionWhole =
      defaultedLimitedLinking compositionWholeOmittedDefaultMultiplicity
    defaultedAssociation =
      defaultedLimitedLinking associationOmittedDefaultMultiplicity
    defaultedInheritance = defaultedLimitedLinking Nothing

selfParticipatesPartWhole
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
selfParticipatesPartWhole Denoted part whole = [iii|
  for #{defaultedLinking part}
  #{which}
  |]
  where
    which = uncurry denotions $ sortLimits
      part {defaultedLinking = "its part end"}
      whole {defaultedLinking = "its whole end"}
selfParticipatesPartWhole Participations part whole = [iii|
  for #{defaultedLinking part}
  where #{participates partIt} as part
  and #{phraseLimitDefault $ defaultedLimits whole} as whole
  |]
  where
    partIt = part {defaultedLinking = "it"}

phraseParticipations
  :: PhrasingKind
  -> DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
phraseParticipations = \case
  Denoted -> denotions
  Participations -> participations

denotions
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
denotions one other = case (defaultedRange one, defaultedRange other) of
  (Nothing, Nothing) -> [iii|which has not denoted multiplicities at all|]
  (Nothing, Just otherRange) -> [iii|
    which has no multiplicity denoted near #{defaultedLinking one}
    and #{otherRange} near #{defaultedLinking other}
    |]
  (Just oneRange, Nothing) -> [iii|
    which has no multiplicity denoted near #{defaultedLinking other}
    and #{oneRange} near #{defaultedLinking one}
    |]
  (Just oneRange, Just otherRange) -> [iii|
    which has denoted the multiplicity
    #{oneRange} near #{defaultedLinking one}
    and #{otherRange} near #{defaultedLinking other}
    |]

participations
  :: DefaultedLimitedLinking
  -> DefaultedLimitedLinking
  -> String
participations one other = [iii|
  where #{participates one}
  and #{participates other}
  |]

participates :: DefaultedLimitedLinking -> String
participates DefaultedLimitedLinking {..}
  = defaultedLinking ++ " participates "
  ++ phraseLimitDefault defaultedLimits

phraseLimitDefault :: Maybe (Int, Maybe Int) -> String
phraseLimitDefault = maybe "with the default multiplicity" phraseLimit

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "not at all"
phraseLimit (1, Just 1)  = "exactly once"
phraseLimit (2, Just 2)  = "exactly twice"
phraseLimit (-1, Just n) = "*.." ++ show n ++ " times"
phraseLimit (m, Nothing) = show m ++ "..* times"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ " times"

=========================

module PhrasingCommon (
  PhrasingStrings (..),
  phraseChangeWith
) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Types (
  AnyRelationship,
  NonInheritancePhrasing (..),
  OmittedDefaultMultiplicities (..),
  PhrasingKind (..),
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))

data PhrasingStrings = PhrasingStrings
  { changeNothing :: String
  , addPrefix :: String
  , removePrefix :: String
  , replacePrefix :: String
  , byInfix :: String
  , postProcess :: String -> String  -- ^ Post-processing function for things like trailing commas
  , phraseRelationWith
      :: OmittedDefaultMultiplicities
      -> ArticleToUse
      -> PhrasingKind
      -> NonInheritancePhrasing
      -> AnyRelationship String String
      -> String
  }

phraseChangeWith
  :: PhrasingStrings
  -> OmittedDefaultMultiplicities
  -> ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChangeWith strings defaultMultiplicities article byName withDir c =
  case (add c, remove c) of
  (Nothing, Nothing) -> changeNothing strings
  (Just e,  Nothing) -> addPrefix strings ++ postProcess strings (phrasingNew e)
  (Nothing, Just e ) -> removePrefix strings ++ phrasingOld e
  (Just e1, Just e2) ->
    replacePrefix strings ++ postProcess strings (phrasingOld e2)
    ++ byInfix strings ++ phrasingNew e1
  where
    phrasingOld = phraseRelationWith strings
      defaultMultiplicities
      article
      Denoted
      $ toPhrasing byName withDir
    phrasingNew = phraseRelationWith strings
      defaultMultiplicities
      IndefiniteArticle
      Participations
      $ toPhrasing False withDir
