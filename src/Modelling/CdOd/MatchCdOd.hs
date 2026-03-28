{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.MatchCdOd (
  MatchCdOdConfig (..),
  MatchCdOdInstance (..),
  MatchCdOdTaskTextElement (..),
  checkMatchCdOdConfig,
  checkMatchCdOdInstance,
  defaultMatchCdOdConfig,
  defaultMatchCdOdInstance,
  getMatchCdOdTask,
  getODInstances,
  matchCdOd,
  matchCdOdEvaluation,
  matchCdOdSolution,
  matchCdOdSyntax,
  matchCdOdTask,
  matchingShow,
  takeRandomInstances,
  ) where

import qualified Modelling.CdOd.CdAndChanges.Transform as Changes (transform)

import qualified Data.Bimap                       as BM (fromList, insert, keys)
import qualified Data.Map                         as M (
  adjust,
  elems,
  filter,
  foldrWithKey,
  fromAscList,
  fromList,
  keys,
  lookup,
  map,
  size,
  toList,
  traverseWithKey,
  )

import Autolib.Hash                     (Hashable)
import Autolib.Reader                   (Reader)
import Autolib.ToDoc                    (ToDoc)
import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (randomise),
  RandomiseLayout (randomiseLayout),
  RandomiseNames (hasRandomisableNames, randomiseNames),
  )
import Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  )
import Modelling.Auxiliary.Shuffle.All  (shuffleEverything)
import Modelling.CdOd.CD2Alloy.Transform (
  LinguisticReuse (None),
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.CdAndChanges.Instance (
  GenericClassDiagramInstance (..),
  fromInstanceWithNameOverlap,
  nameClassDiagramInstance,
  validChangeClassDiagram,
  )
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  )
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Phrasing          (numberWords)
import Modelling.CdOd.Types (
  Cd,
  CdDrawSettings (..),
  CdMutation (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  RelationshipMutation (ChangeKind),
  allCdMutations,
  anonymiseObjects,
  associationNames,
  checkCdDrawSettings,
  checkCdMutations,
  checkClassConfigAndObjectProperties,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  checkObjectProperties,
  checkOmittedDefaultMultiplicities,
  classNames,
  defaultOmittedDefaultMultiplicities,
  defaultProperties,
  fromClassDiagram,
  isObjectDiagramRandomisable,
  linkLabels,
  relationshipName,
  renameClassesAndRelationships,
  renameObjectsWithClassesAndLinksInOd,
  shuffleCdNames,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types (
  Letters (Letters, lettersList),
  showLetters,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Exception                (Exception)
import Control.Monad                    ((<=<), when)
import Control.Monad.Catch              (MonadCatch, MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail               (MonadFail)
#endif
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  ExtraText (..),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  extra,
  german,
  image,
  multipleChoice,
  paragraph,
  reRefuse,
  translate,
  translations,
  Language (English, German),
  )
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Code, Paragraph, Special, Translated),
  )
import Control.OutputCapable.Blocks.Type (
  Output,
  SpecialOutput,
  specialToOutputCapable,
  toOutputCapable,
  )
import Control.Monad.Random (
  MonadRandom,
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans.Random (RandT)
import System.Random (RandomGen)
import Data.Bifunctor                   (Bifunctor (second))
import Data.Bitraversable               (bimapM)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (Forward))
import Data.List                        ((\\), intercalate, singleton, sort)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, isJust, listToMaybe, mapMaybe, fromMaybe)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (iii)
import GHC.Generics                     (Generic)
import Language.Alloy.Call              (AlloyInstance)
import System.Random.Shuffle            (shuffleM)

instance Exception MatchCdOdException

data MatchCdOdException = InvalidMatchCdOdInstance
  deriving Show

data MatchCdOdInstance
  = MatchCdOdInstance {
    cdDrawSettings :: !CdDrawSettings,
    diagrams       :: Map Int Cd,
    hiddenReferenceCd :: Maybe Cd,
    instances      :: Map Char ([Int], Od),
    showSolution   :: !Bool,
    taskText       :: !MatchCdOdTaskText,
    addText        :: ExtraText
  } deriving (Eq, Generic, Hashable, Read, Reader, Show, ToDoc)

data MatchCdOdConfig
  = MatchCdOdConfig {
    allowedCdMutations :: ![CdMutation],
    classConfig      :: ClassConfig,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    objectProperties :: ObjectProperties,
    omittedDefaultMultiplicities :: OmittedDefaultMultiplicities,
    printSolution    :: Bool,
    timeout          :: Maybe Int,
    withNonTrivialInheritance :: Maybe Bool,
    extraText        :: ExtraText
  } deriving (Generic, Read, Reader, Show, ToDoc)

defaultMatchCdOdConfig :: MatchCdOdConfig
defaultMatchCdOdConfig
  = MatchCdOdConfig {
    allowedCdMutations = allCdMutations,
    classConfig  = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 2),
        associationLimits  = (0, Just 1),
        compositionLimits  = (1, Just 1),
        inheritanceLimits  = (1, Just 2),
        relationshipLimits = (3, Just 4)
      },
    maxInstances     = Just 200,
    objectConfig = ObjectConfig {
      linkLimits           = (4, Just 10),
      linksPerObjectLimits = (0, Just 4),
      objectLimits         = (2, 4)
      },
    objectProperties = ObjectProperties {
      anonymousObjectProportion = 1 % 3,
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Nothing
      },
    omittedDefaultMultiplicities = defaultOmittedDefaultMultiplicities,
    printSolution    = True,
    timeout          = Nothing,
    withNonTrivialInheritance = Just True,
    extraText        = NoExtraText
  }

toMatching :: [Int] -> Map Char [Int] -> Map (Int, Char) Bool
toMatching cds m =
  M.fromList [((cd, od), cd `elem` cdList) | cd <- cds, (od, cdList) <- M.toList m]

checkMatchCdOdConfig :: MatchCdOdConfig -> Maybe String
checkMatchCdOdConfig MatchCdOdConfig {..}
  | Just True <- hasSelfLoops objectProperties
  = Just [iii|
    Enforcing self-loops in all object diagrams is not supported.
    You might want to change 'hasSelfLoops' to 'Nothing' in order
    to have self-loops (by chance) in some (or even all) of the object diagrams.
    |]
  | isJust (usesEveryRelationshipName objectProperties)
  , any
    (`elem` allowedCdMutations)
    [AddRelationship, RemoveRelationship, MutateRelationship ChangeKind]
  = Just [iii|
    Setting 'usesEveryRelationshipName' to anything but 'Nothing' is not
    supported, if relationship names are not forcibly the same across all
    class diagrams, i.e. if 'allowedCdMutations' include any of
    'AddRelationship', 'RemoveRelationship' or 'MutateRelationship ChangeKind'.
    |]
  | otherwise
  = checkClassConfigWithProperties classConfig defaultProperties
  <|> checkCdMutations allowedCdMutations
  <|> checkObjectProperties objectProperties
  <|> checkClassConfigAndObjectProperties classConfig objectProperties
  <|> checkOmittedDefaultMultiplicities omittedDefaultMultiplicities

checkMatchCdOdInstance :: MatchCdOdInstance -> Maybe String
checkMatchCdOdInstance MatchCdOdInstance {..}
  | not $ printNames cdDrawSettings
  = Just [iii|printNames has to be set to True for this task type.|]
  | not $ printNavigations cdDrawSettings
  = Just [iii|printNavigations has to be set to True for this task type.|]
  | otherwise
  = foldr ((<>) . checkObjectDiagram . snd) Nothing (M.elems instances)
  <|> checkCdDrawSettings cdDrawSettings

type MatchCdOdTaskText = [SpecialOutput MatchCdOdTaskTextElement]

data MatchCdOdTaskTextElement
  = GivenCds
  | GivenOds
  | DirectionsAdvice Bool
  | SimplifiedInformation Bool
  deriving (Eq, Generic, Hashable, Ord, Read, Reader, Show, ToDoc)

matchCdOdTask
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => Bool
  -> FilePath
  -> MatchCdOdInstance
  -> LangM m
matchCdOdTask showInputHelp path task = do
  toTaskText showInputHelp path task
  hoveringInformation True
  pure ()

toTaskText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => Bool
  -> FilePath
  -> MatchCdOdInstance
  -> LangM m
toTaskText showInputHelp path task = do
  specialToOutputCapable (toTaskSpecificText path task) (taskText task)
  when showInputHelp $
    toOutputCapable (inputHelpText hasGivenCds)
  extra $ addText task
  pure ()
  where
    hasGivenCds = Special GivenCds `elem` taskText task

toTaskSpecificText
  :: (
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    MonadThrow m,
    OutputCapable m
    )
  => FilePath
  -> MatchCdOdInstance
  -> MatchCdOdTaskTextElement
  -> LangM m
toTaskSpecificText path MatchCdOdInstance {..} = \case
  GivenCds -> images show id
    $=<< (\_ cd -> cacheCd cdDrawSettings mempty Nothing (fromClassDiagram cd) path)
    `M.traverseWithKey` diagrams
  GivenOds -> images (:[]) snd
    $=<< (\_ (is,o) -> (is,) <$> cacheOd o Nothing Forward True path)
    `M.traverseWithKey` instances
  DirectionsAdvice b -> directionsAdvice b
  SimplifiedInformation b -> simplifiedInformation b

defaultMatchCdOdTaskText
    :: Int
    -> Int
    -> MatchCdOdTaskText
defaultMatchCdOdTaskText diagramCount instanceCount =  [
  Paragraph $ singleton $ Translated $ translations $ do
    let plural     = diagramCount > 1
        numberWord = fromMaybe (show diagramCount) . M.lookup diagramCount . numberWords

    english $ "Consider the following " ++
              if plural
              then [iii|#{numberWord English} (valid) class diagrams:|]
              else "(valid) class diagram:"
    german  $ "Betrachten Sie " ++
              if plural
              then [iii|die folgenden #{numberWord German} (gültigen) Klassendiagramme:|]
              else "das folgende (gültige) Klassendiagramm:",
  Special GivenCds,
  Paragraph $ singleton $ Translated $ translations $ do
    let plural      = instanceCount > 1
        multipleCds = diagramCount > 1
        numberWord  = fromMaybe (show instanceCount) . M.lookup instanceCount . numberWords

    english $
      (if plural
      then [iii|
        Which of the following #{numberWord English} object diagrams
        conform to #{if multipleCds then "which" else "the"}
        class diagram?|]
      else
        if multipleCds
        then "To which class diagram does the following object diagram conform?"
        else "Does the following object diagram conform to the class diagram?") ++
      if multipleCds
      then [iii|
        \nAn object diagram can conform to none, one,
        or multiple of the given class diagrams.|]
      else ""
    german $
      (if plural
      then [iii|
        Welche der folgenden #{numberWord German} Objektdiagramme
        passen zu #{if multipleCds then "welchem" else "dem"}
        Klassendiagramm?|]
      else
        if multipleCds
        then "Zu welchem Klassendiagramm passt das folgende Objektdiagramm?"
        else "Passt das folgende Objektdiagramm zu dem Klassendiagramm?") ++
      if multipleCds
      then [iii|
        \nEin Objektdiagramm kann zu keinem, einem
        oder mehreren der gegebenen Klassendiagramme passen.|]
      else "",
  Special GivenOds,
  Special $ DirectionsAdvice True,
  Special $ SimplifiedInformation True
  ]

inputHelpText :: Bool -> [Output]
inputHelpText hasGivenCds = [
  Paragraph [
    Translated $ translations $ do
      english [iii|
        State your answer by giving a list of pairs,
        each comprising of a #{entityNameEn} number and any amount of object diagram letters.
        \n
        Each pair indicates that the mentioned object diagrams conform to the
        respective #{entityNameEn}.
        \n
        For example,#{" "}|]
      german [iii|
        Geben Sie Ihre Antwort in Form einer Liste von Paaren an,
        die jeweils aus einer #{entityNameDe}-Nummer und beliebig vielen
        Objektdiagrammbuchstaben bestehen.
        \n
        Jedes Paar gibt an, dass die genannten Objektdiagramme
        zu #{entityNameDeDative} passen.
        \n
        Zum Beispiel drückt#{" "}|],
    Code . uniform . show $ matchingShow matchCdOdInitial,
    Translated $ translations $ do
      english [iii|
        expresses that among the offered choices exactly
        the object diagrams a and b are instances of class diagram #{entityNameEn} 1 and
        that none of the offered object diagrams
        are instances of class diagram #{entityNameEn} 2.
        |]
      german [iii|
        aus, dass unter den angebotenen Auswahlmöglichkeiten
        genau die Objektdiagramme a und b Instanzen #{entityNameDeGenitive} 1 sind
        und dass keines der angebotenen Objektdiagramme
        Instanz #{entityNameDeGenitive} 2 ist.
        |]
    ]
  ]
  where
    (entityNameEn, entityNameDe, entityNameDeDative, entityNameDeGenitive) =
      if hasGivenCds
      then ( "class diagram", "Klassendiagramm"
           , "dem jeweiligen Klassendiagramm", "des Klassendiagramms")
      else ( "scenario description", "Szenariobeschreibung"
           , "der jeweiligen Szenariobeschreibung", "der Szenariobeschreibung")

newtype ShowLetters = ShowLetters { showLetters' :: Letters }

instance Show ShowLetters where
  show = showLetters . showLetters'

matchingShow :: [(Int, Letters)] -> [(Int, ShowLetters)]
matchingShow = map (second ShowLetters)

matchCdOdInitial :: [(Int, Letters)]
matchCdOdInitial = [(1, Letters "ab"), (2, Letters "")]

matchCdOdSyntax
  :: (Foldable t, OutputCapable m)
  => MatchCdOdInstance
  -> t (Int, Letters)
  -> LangM m
matchCdOdSyntax task sub = addPretext $ do
  assertion (all (availableCd . fst) sub) $ translate $ do
    english "Referenced class diagrams were provided within task?"
    german [iii|
      Referenzierte Klassendiagramme sind Bestandteil der Aufgabenstellung?
      |]
  assertion (all (all availableOd . lettersList . snd) sub) $ translate $ do
    english "Referenced object diagrams were provided within task?"
    german "Referenzierte Objektdiagramme sind Bestandteil der Aufgabenstellung?"
  pure ()
  where
    availableCd = (`elem` M.keys (diagrams task))
    availableOd = (`elem` M.keys (instances task))

matchCdOdEvaluation
  :: (
    Alternative m,
    Foldable t,
    MonadCache m,
    MonadDiagrams m,
    MonadGraphviz m,
    OutputCapable m
    )
  => FilePath
  -> MatchCdOdInstance
  -> t (Int, Letters)
  -> Rated m
matchCdOdEvaluation path task@MatchCdOdInstance {..} sub' = do
  let sub = toMatching' sub'
      sol = fst <$> instances
      matching = toMatching (M.keys diagrams) sol
      refOnlyLetters = M.keys $ M.filter null sol
      what = translations $ do
        english "instances"
        german "Instanzen"
      solution =
        if showSolution
        then Just . (DefiniteArticle,) . show . matchingShow
          $ matchCdOdSolution task
        else Nothing
  reRefuse (multipleChoice what solution matching sub) $ do
    when showSolution $
      case hiddenReferenceCd of
        Nothing -> pure ()
        Just cd
          | null refOnlyLetters -> pure ()
          | otherwise -> do
              paragraph $ translate $ do
                english [iii|
                  None of the class diagrams shown above applies to the following object diagram(s):
                  |]
                german [iii|
                  Zu den folgenden Objektdiagrammen passt keines der oben gezeigten Klassendiagramme:
                  |]
              code $ "[" ++ intercalate ", " (map (:[]) $ sort refOnlyLetters) ++ "]"
              paragraph $ translate $ do
                english [iii|
                  Consider the following reference class diagram conforming to them:
                  |]
                german [iii|
                  Betrachten Sie das folgende Referenz-Klassendiagramm, das zu ihnen passt:
                  |]
              image $=<< cacheCd cdDrawSettings mempty Nothing (fromClassDiagram cd) path
              pure ()
  where
    toMatching' :: Foldable f => f (Int, Letters) -> [(Int, Char)]
    toMatching' =
      foldr (\(c, ys) xs -> foldr ((:) . (c,)) xs (lettersList ys)) []

matchCdOdSolution :: MatchCdOdInstance -> [(Int, Letters)]
matchCdOdSolution task = M.toList $ reverseMapping (fst <$> instances task)
  where
    reverseMapping :: Map Char [Int] -> Map Int Letters
    reverseMapping = fmap (fmap Letters) . M.foldrWithKey
      (\x ys xs -> foldr (M.adjust (x:)) xs ys)
      $ M.map (const []) (diagrams task)

matchCdOd
  :: (MonadAlloy m, MonadCatch m, MonadFail m)
  => MatchCdOdConfig
  -> Int
  -> Int
  -> m MatchCdOdInstance
matchCdOd config segment seed = flip evalRandT g $ do
  inst <- getMatchCdOdTask getRandomTask config
  shuffleEverything inst
  where
    g = mkStdGen $ (segment +) $ 4 * seed

getMatchCdOdTask
  :: (MonadCatch m, RandomGen g)
  => (MatchCdOdConfig
    -> RandT g m (Map Int Cd, Cd, Map Char ([Int], AlloyInstance)))
  -> MatchCdOdConfig
  -> RandT g m MatchCdOdInstance
getMatchCdOdTask f config@MatchCdOdConfig {..} = do
  (cds, hiddenReferenceCd, ods) <- f config
  let possibleLinkNames = concatMap
        (mapMaybe relationshipName . relationships)
        cds
  ods' <- mapM (mapM $ toOd possibleLinkNames) ods
  return $ MatchCdOdInstance {
        cdDrawSettings = CdDrawSettings {
          omittedDefaults = omittedDefaultMultiplicities,
          printNames = True,
          printNavigations = True
          },
        diagrams       = cds,
        hiddenReferenceCd = Just hiddenReferenceCd,
        instances      = ods',
        showSolution = printSolution,
        taskText = defaultMatchCdOdTaskText (M.size cds) (M.size ods'),
        addText = extraText
        }
  where
    toOd possibleLinkNames =
      anonymiseObjects (anonymousObjectProportion objectProperties)
      <=< lift . alloyInstanceToOd Nothing possibleLinkNames

{-|
A 'defaultMatchCdOdInstance' as generated using 'defaultMatchCdOdConfig'.
-}
defaultMatchCdOdInstance :: MatchCdOdInstance
defaultMatchCdOdInstance = MatchCdOdInstance {
  cdDrawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = True
    },
  diagrams = M.fromList [
    (1, ClassDiagram {
      classNames = ["C", "D", "B", "A"],
      relationships = [
        Aggregation {
          aggregationName = "z",
          aggregationPart = LimitedLinking {
            linking = "B",
            limits = (0, Just 2)
            },
          aggregationWhole = LimitedLinking {
            linking = "A",
            limits = (1, Nothing)
            }
          },
        Association {
          associationName = "w",
          associationFrom = LimitedLinking {
            linking = "C",
            limits = (1, Nothing)
            },
          associationTo = LimitedLinking {
            linking = "D",
            limits = (1, Nothing)
            }
          },
        Composition {
          compositionName = "x",
          compositionPart = LimitedLinking {
            linking = "D",
            limits = (1, Just 2)
            },
          compositionWhole = LimitedLinking {
            linking = "A",
            limits = (0, Just 1)
            }
          },
        Inheritance {
          subClass = "C",
          superClass = "A"
          }
        ]
      }),
    (2, ClassDiagram {
      classNames = ["B", "D", "A", "C"],
      relationships = [
        Association {
          associationName = "w",
          associationFrom = LimitedLinking {
            linking = "C",
            limits = (1, Nothing)
            },
          associationTo = LimitedLinking {
            linking = "D",
            limits = (1, Nothing)
            }
          },
        Aggregation {
          aggregationName = "z",
          aggregationPart = LimitedLinking {
            linking = "B",
            limits = (0, Just 2)
            },
          aggregationWhole = LimitedLinking {
            linking = "A",
            limits = (1, Nothing)
            }
          },
        Composition {
          compositionName = "x",
          compositionPart = LimitedLinking {
            linking = "A",
            limits = (2, Nothing)
            },
          compositionWhole = LimitedLinking {
            linking = "D",
            limits = (0, Just 1)
            }
          },
        Inheritance {
          subClass = "C",
          superClass = "A"
          }
        ]
      })
    ],
  hiddenReferenceCd  = Just $ ClassDiagram {
    classNames = ["A", "C", "D", "B"],
    relationships = [
      Composition {
        compositionName = "x",
        compositionPart = LimitedLinking {
          linking = "A",
          limits = (1, Just 2)
          },
        compositionWhole = LimitedLinking {
          linking = "D",
          limits = (0, Just 1)
          }
        },
      Aggregation {
        aggregationName = "w",
        aggregationPart = LimitedLinking {
          linking = "C",
          limits = (1, Nothing)
          },
        aggregationWhole = LimitedLinking {
          linking = "D",
          limits = (1, Nothing)
          }
        },
      Inheritance {
        subClass = "C",
        superClass = "A"
        },
      Aggregation {
        aggregationName = "z",
        aggregationPart = LimitedLinking {
          linking = "B",
          limits = (0, Just 2)
          },
        aggregationWhole = LimitedLinking {
          linking = "A",
          limits = (1, Nothing)
          }
        }
        ]
      },
  instances = M.fromList [
    ('a', ([1], ObjectDiagram {
      objects = [
        Object {isAnonymous = True, objectName = "b1", objectClass = "B"},
        Object {isAnonymous = False, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "b", objectClass = "B"},
        Object {isAnonymous = False, objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "z", linkFrom = "b1", linkTo = "c"},
        Link {linkLabel = "z", linkFrom = "b", linkTo = "c"},
        Link {linkLabel = "x", linkFrom = "d", linkTo = "c"}
        ]
      })),
    ('b', ([], ObjectDiagram {
      objects = [
        Object {isAnonymous = True, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"},
        Object {isAnonymous = False, objectName = "d", objectClass = "D"},
        Object {isAnonymous = False, objectName = "b", objectClass = "B"}
        ],
      links = [
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "z", linkFrom = "b", linkTo = "c"},
        Link {linkLabel = "z", linkFrom = "b", linkTo = "a"},
        Link {linkLabel = "x", linkFrom = "a", linkTo = "d"}
        ]
      })),
    ('c', ([2], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "d", objectClass = "D"},
        Object {isAnonymous = True, objectName = "a1", objectClass = "A"},
        Object {isAnonymous = False, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"}
        ],
      links = [
        Link {linkLabel = "x", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "x", linkFrom = "a", linkTo = "d"},
        Link {linkLabel = "x", linkFrom = "a1", linkTo = "d"}
        ]
      })),
    ('d', ([2], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "d", objectClass = "D"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"},
        Object {isAnonymous = True, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "c1", objectClass = "C"}
        ],
      links = [
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "w", linkFrom = "c1", linkTo = "d"},
        Link {linkLabel = "x", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "x", linkFrom = "c1", linkTo = "d"}
        ]
      })),
    ('e', ([1], ObjectDiagram {
      objects = [
        Object {isAnonymous = False, objectName = "c", objectClass = "C"},
        Object {isAnonymous = False, objectName = "a", objectClass = "A"},
        Object {isAnonymous = True, objectName = "d1", objectClass = "D"},
        Object {isAnonymous = False, objectName = "d", objectClass = "D"}
        ],
      links = [
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d1"},
        Link {linkLabel = "w", linkFrom = "c", linkTo = "d"},
        Link {linkLabel = "x", linkFrom = "d", linkTo = "a"},
        Link {linkLabel = "x", linkFrom = "d1", linkTo = "c"}
        ]
      }))
    ],
  showSolution = True,
  taskText = defaultMatchCdOdTaskText 2 5,
  addText = NoExtraText
  }

classAndNonInheritanceNames :: MatchCdOdInstance -> ([String], [String])
classAndNonInheritanceNames inst =
  let names = nubOrd $ concatMap classNames (diagrams inst)
      nonInheritances = nubOrd $ concatMap associationNames (diagrams inst)
        ++ concatMap (linkLabels . snd) (instances inst)
  in (names, nonInheritances)

instance Randomise MatchCdOdInstance where
  randomise = shuffleInstance

instance RandomiseNames MatchCdOdInstance where
  randomiseNames inst = do
    let (names, nonInheritances) = classAndNonInheritanceNames inst
    names'  <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    lift $ renameInstance inst names' nonInheritances'

  hasRandomisableNames MatchCdOdInstance {..} = listToMaybe
    $ mapMaybe (isObjectDiagramRandomisable . snd) $ M.elems instances

instance RandomiseLayout MatchCdOdInstance where
  randomiseLayout = shuffleNodesAndEdges

shuffleNodesAndEdges
  :: MonadRandom m
  => MatchCdOdInstance
  -> m MatchCdOdInstance
shuffleNodesAndEdges MatchCdOdInstance {..} = do
  cds <- mapM shuffleClassAndConnectionOrder diagrams
  hiddenReferenceCd' <- mapM shuffleClassAndConnectionOrder hiddenReferenceCd
  ods <- mapM (mapM shuffleObjectAndLinkOrder) instances
  return MatchCdOdInstance {
    cdDrawSettings = cdDrawSettings,
    diagrams = cds,
    hiddenReferenceCd = hiddenReferenceCd',
    instances = ods,
    showSolution = showSolution,
    taskText = taskText,
    addText = addText
    }

shuffleInstance
  :: (MonadThrow m, RandomGen g)
  => MatchCdOdInstance
  -> RandT g m MatchCdOdInstance
shuffleInstance MatchCdOdInstance {..} = do
  cds <- shuffleM $ M.toList diagrams
  ods <- shuffleM $ M.toList instances
  let changeId x (y, cd) = ((y, x), (x, cd))
      (idMap, cds') = unzip $ zipWith changeId [1..] cds
      replaceId x (_, od) = (x, od)
      rename = maybe (lift $ throwM InvalidMatchCdOdInstance) return
        . (`lookup` idMap)
  ods' <- mapM (mapM $ bimapM (mapM rename) return)
    $ zipWith replaceId ['a'..] ods
  return $ MatchCdOdInstance {
    cdDrawSettings = cdDrawSettings,
    diagrams = M.fromAscList cds',
    hiddenReferenceCd = hiddenReferenceCd,
    instances = M.fromAscList ods',
    showSolution = showSolution,
    taskText = taskText,
    addText = addText
    }

renameInstance
  :: MonadThrow m
  => MatchCdOdInstance
  -> [String]
  -> [String]
  -> m MatchCdOdInstance
renameInstance inst@MatchCdOdInstance {..} names' nonInheritances' = do
  let (names, nonInheritances) = classAndNonInheritanceNames inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      bmWithIdForUnmappedKeys bm ks =
        foldr (\k -> BM.insert k k) bm (ks \\ BM.keys bm)
      bmNamesForReferenceCd =
        bmWithIdForUnmappedKeys bmNames (maybe [] classNames hiddenReferenceCd)
      bmNonInheritancesForReferenceCd =
        bmWithIdForUnmappedKeys bmNonInheritances (maybe [] associationNames hiddenReferenceCd)
      renameCd = renameClassesAndRelationships bmNames bmNonInheritances
      renameOd = renameObjectsWithClassesAndLinksInOd bmNames bmNonInheritances
      renameReferenceCd =
        renameClassesAndRelationships bmNamesForReferenceCd bmNonInheritancesForReferenceCd
  cds <- renameCd `mapM` diagrams
  hiddenReferenceCd' <- renameReferenceCd `mapM` hiddenReferenceCd
  ods <- mapM renameOd `mapM` instances
  return $ MatchCdOdInstance {
    cdDrawSettings = cdDrawSettings,
    diagrams = cds,
    hiddenReferenceCd = hiddenReferenceCd',
    instances = ods,
    showSolution = showSolution,
    taskText = taskText,
    addText = addText
    }

getRandomTask
  :: (MonadAlloy m, MonadFail m, RandomGen g, MonadThrow m)
  => MatchCdOdConfig
  -> RandT g m (Map Int Cd, Cd, Map Char ([Int], AlloyInstance))
getRandomTask config = do
  let alloyCode = Changes.transform
        (classConfig config)
        (allowedCdMutations config)
        defaultProperties
        (withNonTrivialInheritance config)
  alloyInstances <- lift $ getInstances (maxInstances config) (timeout config) alloyCode
  randomInstances <- shuffleM alloyInstances
  ods <- getODsFor config { timeout = Nothing } randomInstances
  maybe (error "could not find instance") return ods

getODsFor
  :: (MonadAlloy m, MonadFail m, RandomGen g, MonadThrow m)
  => MatchCdOdConfig
  -> [AlloyInstance]
  -> RandT g m (Maybe (Map Int Cd, Cd, Map Char ([Int], AlloyInstance)))
getODsFor _      []       = return Nothing
getODsFor config (cd:cds) = do
  cds' <- lift (instanceChangesAndCds
    <$> (nameClassDiagramInstance <=< fromInstanceWithNameOverlap) cd
    )
  cds'' <- lift $ mapM validChangeClassDiagram cds'
  [cd1', cd2', cd3] <- mapM shuffleClassAndConnectionOrder cds''
    >>= shuffleCdNames
  [cd1, cd2] <- shuffleM [cd1', cd2']
  alloyInstances <- lift $ getODInstances config cd1 cd2 cd3 $ length $ classNames cd1
  maybeRandomInstances <- takeRandomInstances alloyInstances
  case maybeRandomInstances of
    Nothing      -> getODsFor config cds
    Just randomInstances -> return $ Just (
      M.fromList [(1, cd1), (2, cd2)],
      cd3,
      M.fromList $ zip ['a' ..] randomInstances
      )

getODInstances
  :: MonadAlloy m
  => MatchCdOdConfig
  -> Cd
  -> Cd
  -> Cd
  -> Int
  -> m (Map [Int] [AlloyInstance])
getODInstances config cd1 cd2 cd3 numClasses = do
  let parts1 = alloyFor cd1 "1"
      parts2 = alloyFor cd2 "2"
      parts1and2 = mergeParts parts1 parts2
      combined1and2 = combineParts parts1and2
      parts3 = alloyFor cd3 "3"
      parts1to3 = mergeParts parts1and2 parts3
      relationships1and2 = relationships cd1 ++ relationships cd2
      relationships1to3 = relationships1and2 ++ relationships cd3
      allRelationshipNames = mapMaybe relationshipName relationships1to3
      alloyFor = alloyForAllRelationships allRelationshipNames
      cd1not2 = runCommand "cd1 and (not cd2)" relationships1and2
      cd2not1 = runCommand "cd2 and (not cd1)" relationships1and2
      cd1and2 = runCommand "cd1 and cd2" relationships1and2
      cdNot1not2 = runCommand
        "(not cd1) and (not cd2) and cd3"
        relationships1to3
  instances1not2 <- getInstances maxIs to (combined1and2 ++ cd1not2)
  instances2not1 <- getInstances maxIs to (combined1and2 ++ cd2not1)
  instances1and2 <- getInstances maxIs to (combined1and2 ++ cd1and2)
  instancesNot1not2 <-
    getInstances maxIs to (combineParts parts1to3 ++ cdNot1not2)
  return $ M.fromList [([1]  , instances1not2),
                       ([2]  , instances2not1),
                       ([1,2], instances1and2),
                       ([]   , instancesNot1not2)]
  where
    alloyForAllRelationships allRelationshipNames cd nr = transform
      None
      cd
      (Just allRelationshipNames)
      []
      (objectConfig config)
      (objectProperties config)
      nr
      ""
    to = timeout config
    maxIs = maxInstances config
    runCommand x = createRunCommand
      x
      Nothing
      numClasses
      (objectConfig config)

takeRandomInstances
  :: (MonadRandom m, MonadFail m) => Map [Int] [a] -> m (Maybe [([Int], a)])
takeRandomInstances alloyInstances =
  case takes of
    []  -> return Nothing
    _:_ -> Just <$> do
      randomInstances <- mapM shuffleM alloyInstances
      ts:_    <- shuffleM takes
      shuffleM $ concatMap ($ randomInstances) ts
  where
    takes =
      [ [takeL [1] x, takeL [2] y, takeL [1,2] z, takeL [] u]
      | x <- [0 .. min 2 (length $ fromJust $ M.lookup [1]   alloyInstances)]
      , y <- [0 .. min 2 (length $ fromJust $ M.lookup [2]   alloyInstances)]
      , z <- [0 .. min 2 (length $ fromJust $ M.lookup [1,2] alloyInstances)]
      , u <- [0 .. min 2 (length $ fromJust $ M.lookup []    alloyInstances)]
      , 5 == x + y + z + u
      ]
    takeL k n = take n . fmap (k,) . fromJust . M.lookup k
