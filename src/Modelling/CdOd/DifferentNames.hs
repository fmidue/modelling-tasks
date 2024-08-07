{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (..),
  DifferentNamesInstance (..),
  ShufflingOption (..),
  checkDifferentNamesConfig,
  checkDifferentNamesInstance,
  defaultDifferentNamesConfig,
  defaultDifferentNamesInstance,
  differentNames,
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSolution,
  differentNamesSyntax,
  differentNamesTask,
  getDifferentNamesTask,
  mappingShow,
  renameInstance,
  ) where

import qualified Modelling.CdOd.Types             as T (CdDrawSettings (..))

import qualified Data.Bimap                       as BM (
  filter,
  fromList,
  keysR,
  lookup,
  lookupR,
  toAscList,
  twist,
  )
import qualified Data.Map                         as M (
  fromAscList,
  )
import qualified Data.Set                         as S (toList)

import Capabilities.Alloy               (MonadAlloy, getInstances)
import Capabilities.Cache               (MonadCache)
import Capabilities.Diagrams            (MonadDiagrams)
import Capabilities.Graphviz            (MonadGraphviz)
import Modelling.Auxiliary.Common (
  Randomise (isRandomisable, randomise),
  RandomiseLayout (randomiseLayout),
  TaskGenerationException (NoInstanceAvailable),
  shuffleEverything,
  )
import Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  )
import Modelling.CdOd.Auxiliary.Util
import Modelling.CdOd.CD2Alloy.Transform (
  combineParts,
  createRunCommand,
  mergeParts,
  transform,
  )
import Modelling.CdOd.Generate          (generateCds, instanceToCd)
import Modelling.CdOd.Output            (cacheCd, cacheOd)
import Modelling.CdOd.Types (
  Cd,
  CdDrawSettings,
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  ObjectProperties (..),
  Od,
  Relationship (..),
  associationNames,
  checkCdDrawSettings,
  checkClassConfigWithProperties,
  checkObjectDiagram,
  classNames,
  defaultDrawSettings,
  defaultProperties,
  isObjectDiagramRandomisable,
  linkNames,
  relationshipName,
  renameObjectsWithClassesAndLinksInOd,
  renameClassesAndRelationships,
  reverseAssociation,
  shuffleClassAndConnectionOrder,
  shuffleObjectAndLinkOrder,
  )
import Modelling.Types (
  Name (Name),
  NameMapping (nameMapping),
  fromNameMapping,
  showName,
  toNameMapping,
  )

import Control.Applicative              (Alternative ((<|>)))
import Control.Monad.Catch              (MonadThrow, throwM)
import Control.Monad.Extra              (whenJust)
import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  Rated,
  ($=<<),
  english,
  german,
  multipleChoice,
  translations,
  translate,
  yesNo,
  )
import Control.Monad.Random (
  MonadRandom (getRandom),
  evalRandT,
  mkStdGen,
  )
import Control.Monad.Trans.Except       (runExceptT)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Bimap                       (Bimap)
import Data.Bitraversable               (bitraverse)
import Data.Bool                        (bool)
import Data.Containers.ListUtils        (nubOrd)
import Data.GraphViz                    (DirType (Back))
import Data.List                        (group, intersect, permutations, sort)
import Data.Maybe (
  fromMaybe,
  isJust,
  isNothing,
  listToMaybe,
  mapMaybe,
  )
import Data.String.Interpolate          (i, iii)
import Data.Tuple.Extra                 (snd3, swap)
import GHC.Generics                     (Generic)
import Language.Alloy.Call (
  AlloyInstance,
  getTripleAs,
  lookupSig,
  scoped,
  )
import System.Random.Shuffle            (shuffleM)

data ShufflingOption a =
    ConsecutiveLetters
  | WithAdditionalNames [a]
  deriving (Eq, Generic, Foldable, Functor, Read, Show, Traversable)

data DifferentNamesInstance = DifferentNamesInstance {
    anonymousObjects :: Bool,
    cDiagram :: Cd,
    generatorValue :: Int,
    oDiagram :: Od,
    showSolution :: Bool,
    mapping  :: NameMapping,
    linkShuffling :: ShufflingOption String,
    -- | whether every relationship has an associated link (in the mapping)
    usesAllRelationships :: Bool
  } deriving (Eq, Generic, Read, Show)

cdDrawSettings :: DifferentNamesInstance -> CdDrawSettings
cdDrawSettings _ = defaultDrawSettings

checkDifferentNamesInstance :: DifferentNamesInstance -> Maybe String
checkDifferentNamesInstance inst@DifferentNamesInstance {..}
  | not $ T.printNames $ cdDrawSettings inst
  = Just [iii|printNames has to be set to True for this task type.|]
  | not $ T.printNavigations $ cdDrawSettings inst
  = Just [iii|printNavigations has to be set to True for this task type.|]
  | WithAdditionalNames xs <- linkShuffling
  , length associations > length links + length xs
  = Just [iii|
      WithAdditionalNames must provide at least a name for
      each missing link in the Object diagram,
      i.e., for which an association in the Class diagram exists
      but not a link in the Object diagram.
      |]
  | (x:_) <- nubOrd links `intersect` nubOrd associations
  = Just [iii|
      Link names and association names must be disjoint
      but currently "#{x}" is among both.
      |]
  | otherwise
  = checkObjectDiagram oDiagram
  <|> checkCdDrawSettings (cdDrawSettings inst)
  where
    associations = associationNames cDiagram
    links = linkNames oDiagram

data DifferentNamesConfig = DifferentNamesConfig {
    classConfig      :: ClassConfig,
    withNonTrivialInheritance :: Maybe Bool,
    maxInstances     :: Maybe Integer,
    objectConfig     :: ObjectConfig,
    objectProperties :: ObjectProperties,
    onlyAnonymousObjects :: Bool,
    -- | whether to enforce one relationship not matching to
    -- any link in the object diagram
    ignoreOneRelationship :: Maybe Bool,
    printSolution    :: Bool,
    timeout          :: Maybe Int
  } deriving (Generic, Read, Show)

checkDifferentNamesConfig :: DifferentNamesConfig -> Maybe String
checkDifferentNamesConfig DifferentNamesConfig {..}
  | (x, Just y) <- relationshipLimits classConfig, x /= y
  = Just [iii|
      The minimum number of relationships has to equal its maximum number
      for this task type.
      Otherwise task instances would vary too much in complexity.
      |]
  | (x, Just y) <- inheritanceLimits classConfig, x /= y
  = Just [iii|
      The minimum number of inheritances has to equal its maximum number
      for this task type.
      Otherwise task instances could vary too much in difficulty.
      |]
  | isNothing . snd $ relationshipLimits classConfig
  , any
      (different . ($ classConfig))
      [aggregationLimits, associationLimits, compositionLimits]
  = Just [iii|
      The minimum number and maximum number
      of aggregations, associations or compositions may not vary
      if the maximum number of relationships is not fixed.
      Otherwise task instances would vary too much in complexity.
      |]
  | fmap not (usesEveryRelationshipName objectProperties)
    /= ignoreOneRelationship
  = Just [iii|
      If 'ignoreOneRelationship' is set, 'usesEveryRelationshipName' must not
      and vice versa. Or both have to be set to 'Nothing'.
      |]
  | otherwise = checkClassConfigWithProperties classConfig defaultProperties
  where
    different (_, Nothing) = True
    different (x, Just y)  = x /= y

defaultDifferentNamesConfig :: DifferentNamesConfig
defaultDifferentNamesConfig = DifferentNamesConfig {
    classConfig  = ClassConfig {
        classLimits        = (4, 4),
        aggregationLimits  = (1, Just 1),
        associationLimits  = (1, Just 1),
        compositionLimits  = (1, Just 1),
        inheritanceLimits  = (1, Just 1),
        relationshipLimits = (4, Just 4)
      },
    objectConfig = ObjectConfig {
      linkLimits           = (5, Just 10),
      linksPerObjectLimits = (0, Just 3),
      objectLimits         = (4, 6)
      },
    objectProperties = ObjectProperties {
      completelyInhabited = Nothing,
      hasLimitedIsolatedObjects = True,
      hasSelfLoops = Nothing,
      usesEveryRelationshipName = Just True
      },
    onlyAnonymousObjects = True,
    ignoreOneRelationship = Just False,
    printSolution    = False,
    withNonTrivialInheritance = Just True,
    maxInstances     = Nothing,
    timeout          = Nothing
  }

newtype ShowName = ShowName { showName' :: Name }

instance Show ShowName where
  show = showName . showName'

mappingShow :: [(Name, Name)] -> [(ShowName, ShowName)]
mappingShow = fmap (bimap ShowName ShowName)

differentNamesTask
  :: (MonadCache m, MonadDiagrams m, MonadGraphviz m, MonadThrow m, OutputCapable m)
  => FilePath
  -> DifferentNamesInstance
  -> LangM m
differentNamesTask path task = do
  let cd = cDiagram task
      od = oDiagram task
      anonymous = fromMaybe (length (objects od) `div` 3)
        (if anonymousObjects task then Just 1000 else Nothing)
  paragraph $ translate $ do
    english "Consider the following class diagram:"
    german "Betrachten Sie folgendes Klassendiagramm:"
  paragraph $ image $=<< cacheCd (cdDrawSettings task) mempty cd path
  paragraph $ translate $ do
    english "and the following object diagram (which conforms to it):"
    german "und das folgende (dazu passende) Objektdiagramm:"
  paragraph $ image $=<<
    flip evalRandT (mkStdGen $ generatorValue task)
    $ cacheOd od anonymous Back True path
  paragraph $ do
    translate $ do
      english [iii|
        Which relationship in the class diagram (CD) corresponds
        to which of the links in the object diagram (OD)?
        \n
        State your answer by giving a mapping of
        relationships in the CD to links in the OD.
        \n
        To state that a in the CD corresponds to x in the OD and
        b in the CD corresponds to y in the OD write it as:
        |]
      german [iii|
        Welche Beziehung im Klassendiagramm (CD)
        entspricht welchen Links im Objektdiagramm (OD)?
        \n
        Geben Sie Ihre Antwort als eine Zuordnung von
        Beziehungen im CD zu Links im OD an.
        \n
        Um anzugeben, dass a im CD zu x im OD und b im CD
        zu y im OD korrespondieren, schreiben Sie es als:
        |]
    code . show $ mappingShow differentNamesInitial
    pure ()
  paragraph $ translate $ do
    english [iii|
      Please note: Links are already grouped correctly and fully,
      i.e., all links with the same name (and only links with the same name!)
      in the OD correspond to exactly the same relationship name in the CD.
      |]
    german [iii|
      Bitte beachten Sie: Links sind bereits vollständig und korrekt gruppiert,
      d.h., alle Links mit dem selben Namen
      (and auch nur Links mit dem selben Namen!)
      im OD entsprechen genau dem selben Beziehungsnamen im CD.
      |]
  paragraph $ translate $ if usesAllRelationships task
    then do
    english [iii|
      Thus, every link name and every relationship name should occur
      exactly once in your mapping.
      |]
    german [iii|
      Deshalb sollte jeder Linkname und jeder Beziehungsname
      genau einmal in Ihrer Zuordnung auftauchen.
      |]
    else do
    english [i|Thus, every link name should occur exactly once in your mapping.|]
    german [iii|
      Deshalb sollte jeder Linkname
      genau einmal in Ihrer Zuordnung auftauchen.
      |]
    pure ()
  paragraph simplifiedInformation
  paragraph directionsAdvice
  paragraph hoveringInformation
  pure ()

differentNamesInitial :: [(Name, Name)]
differentNamesInitial = bimap Name Name <$> [("a", "x"), ("b", "y")]

differentNamesSyntax
  :: OutputCapable m
  => DifferentNamesInstance
  -> [(Name, Name)]
  -> LangM m
differentNamesSyntax DifferentNamesInstance {..} cs = addPretext $ do
  yesNo (null invalidMappings) $ translate $ do
    english [iii|
      All provided pairs are matching an existing relationship
      and an existing link?
      |]
    german [iii|
      Alle angegebenen Paare ordnen einen vorhandenen Link
      einer vorhandenen Beziehung zu?
      |]
  whenJust (listToMaybe invalidMappings) $ \x ->
    refuse $ paragraph $ translate $ do
      let y = bimap ShowName ShowName x
      english [i|The mapping '#{y}' uses a non-existing identifier.|]
      german [iii|
        Die Zuordnung '#{y}' benutzt einen nicht vorhandenen Bezeichner.
        |]
  yesNo (null allMappingValues) $ translate $ do
    english "All provided pairs are non-overlapping?"
    german "Alle angegebenen Paare sind nicht überlappend?"
  case allMappingValues of
    (x:_):_ -> refuse $ paragraph $ translate $ do
      let y = ShowName x
      english [i|The identifier '#{y}' appears twice within the given mappings.|]
      german [i|
        Der Bezeichner '#{y}' existiert doppelt in den angegebenen Zuordnungen.
        |]
    _ -> pure ()
  pure ()
  where
    links = linkNames oDiagram
    associations = associationNames cDiagram
    isAssociationMappingForward (Name x, Name y) =
      x `elem` associations && y `elem` links
    isAssociationMapping x = isAssociationMappingForward x
      || isAssociationMappingForward (swap x)
    invalidMappings = filter (not . isAssociationMapping) cs
    allMappingValues = filter
      (not . null . tail)
      $ group $ sort (map fst cs ++ map snd cs)

readMapping :: Ord a => Bimap a a -> (a, a) -> Maybe (a, a)
readMapping m (x, y)
  | isJust $ BM.lookup x m, isJust $ BM.lookupR y m
  = Just (x, y)
  | isJust $ BM.lookup y m, isJust $ BM.lookupR x m
  = Just (y, x)
  | otherwise
  = Nothing

differentNamesEvaluation
  :: OutputCapable m
  => DifferentNamesInstance
  -> [(Name, Name)]
  -> Rated m
differentNamesEvaluation task cs = do
  let what = translations $ do
        german "Zuordnungen"
        english "mappings"
      m = nameMapping $ mapping task
      ms = M.fromAscList $ map (,True) $ BM.toAscList m
      solution =
        if showSolution task
        then Just . show . mappingShow $ differentNamesSolution task
        else Nothing
  multipleChoice DefiniteArticle what solution ms (mapMaybe (readMapping m) cs)

differentNamesSolution :: DifferentNamesInstance -> [(Name, Name)]
differentNamesSolution = BM.toAscList . nameMapping . mapping

differentNames
  :: (MonadAlloy m, MonadThrow m)
  => DifferentNamesConfig
  -> Int
  -> Int
  -> m DifferentNamesInstance
differentNames config segment seed = do
  let g = mkStdGen (segment + 4 * seed)
  flip evalRandT g $ do
    is <- generateCds
      (withNonTrivialInheritance config)
      (classConfig config)
      defaultProperties
      (maxInstances config)
      (timeout config)
    tryGettingValidInstanceFor is
  where
    tryGettingValidInstanceFor []             = throwM NoInstanceAvailable
    tryGettingValidInstanceFor (inst:instances) = do
      cd <- instanceToCd inst
      taskInstance <- getDifferentNamesTask
        (tryGettingValidInstanceFor instances)
        config
        cd
      shuffleEverything taskInstance

defaultDifferentNamesInstance :: DifferentNamesInstance
defaultDifferentNamesInstance = DifferentNamesInstance {
  anonymousObjects = True,
  cDiagram = ClassDiagram {
    classNames = ["A","D","B","C"],
    relationships = [
      Inheritance {subClass = "D", superClass = "A"},
      Inheritance {subClass = "C", superClass = "D"},
      Aggregation {
        aggregationName = "b",
        aggregationPart = LimitedLinking {
          linking = "B",
          limits = (1,Just 1)
          },
        aggregationWhole = LimitedLinking {
          linking = "D",
          limits = (0,Nothing)
          }
         },
      Association {
        associationName = "c",
        associationFrom = LimitedLinking {
          linking = "C",
          limits = (0,Just 2)
          },
        associationTo = LimitedLinking {
          linking = "A",
          limits = (0,Just 2)
          }
         },
      Composition {
        compositionName = "a",
        compositionPart = LimitedLinking {
          linking = "B",
          limits = (2,Nothing)
          },
        compositionWhole = LimitedLinking {
          linking = "A",
          limits = (1,Just 1)
          }
        }
      ]
    },
  generatorValue = -3894126834283525023,
  oDiagram = ObjectDiagram {
    objects = [
      Object {objectName = "c", objectClass = "C"},
      Object {objectName = "b", objectClass = "B"},
      Object {objectName = "b1", objectClass = "B"},
      Object {objectName = "b2", objectClass = "B"}
      ],
    links = [
      Link {linkName = "y", linkFrom = "c", linkTo = "b"},
      Link {linkName = "y", linkFrom = "c", linkTo = "b1"},
      Link {linkName = "y", linkFrom = "c", linkTo = "b2"},
      Link {linkName = "x", linkFrom = "c", linkTo = "c"},
      Link {linkName = "z", linkFrom = "c", linkTo = "b2"}
      ]
    },
  showSolution = False,
  mapping = toNameMapping $ BM.fromList [("a","y"),("b","z"),("c","x")],
  linkShuffling = ConsecutiveLetters,
  usesAllRelationships = True
  }

getDifferentNamesTask
  :: (MonadAlloy m, MonadRandom m, MonadThrow m)
  => m DifferentNamesInstance
  -> DifferentNamesConfig
  -> Cd
  -> m DifferentNamesInstance
getDifferentNamesTask tryNext config cd' = do
    let cd     = cd' {
          relationships = map reverseAssociation $ relationships cd'
          }
        cd0    = (0 :: Integer, cd)
        parts0 = uncurry alloyFor cd0
        labels = mapMaybe relationshipName . relationships $ snd cd0
        cds    = map
          (flip renameEdges cd . BM.fromList . zip labels)
          $ drop 1 (permutations labels)
        cds'   = zip [1 :: Integer ..] cds
        partsList = map (uncurry alloyFor) cds'
        runCmd = foldr (\(n, _) -> (++ " and (not cd" ++ show n ++ ")")) "cd0" cds'
        onlyCd0 = createRunCommand
          runCmd
          (length $ classNames cd)
          (objectConfig config)
          (concatMap relationships cds)
          partsList'
        partsList' = foldr mergeParts parts0 partsList
    instances  <- getInstances
      (maxInstances config)
      (timeout config)
      (combineParts partsList' ++ onlyCd0)
    instances' <- shuffleM (instances :: [AlloyInstance])
    continueWithHead instances' $ \od1 -> do
      labels' <- shuffleM labels
      used <- usedLabels od1
      let bm  = BM.fromList $ zip (map (:[]) ['a', 'b' ..]) labels'
          cd1 = renameEdges (BM.twist bm) cd'
          bm' = BM.filter (const (`elem` used)) bm
          isCompleteMapping = BM.keysR bm == sort used
      if maybe (const True) (bool id not) (ignoreOneRelationship config)
         isCompleteMapping
        then do
        od1' <- either error id <$> runExceptT (alloyInstanceToOd od1)
        return $ DifferentNamesInstance {
              anonymousObjects = onlyAnonymousObjects config,
              cDiagram  = cd1,
              generatorValue = 0,
              oDiagram  = od1',
              showSolution = printSolution config,
              mapping   = toNameMapping bm',
              linkShuffling = ConsecutiveLetters,
              usesAllRelationships = isCompleteMapping
              }
        else tryNext
  where
    renameEdges bm = either (error . show) id . bitraverse pure (`BM.lookup` bm)
    alloyFor n cd = transform
      cd
      []
      (objectConfig config)
      (objectProperties config)
      (show n)
      ""
    continueWithHead []    _ = tryNext
    continueWithHead (x:_) f = f x
    usedLabels :: MonadThrow m => AlloyInstance -> m [String]
    usedLabels inst = do
      let ignore = const $ const $ return ()
          name = const . return
      os    <- lookupSig (scoped "this" "Object") inst
      map snd3 . S.toList <$> getTripleAs "get" ignore name ignore os

{-|
All names within a 'DifferentNamesInstance'
including names reserved for shuffling.
-}
classNonInheritanceAndLinkNames
  :: DifferentNamesInstance
  -> ([String], [String], [String])
classNonInheritanceAndLinkNames DifferentNamesInstance {..} =
  let names = classNames cDiagram
      nonInheritances = associationNames cDiagram
      additional = case linkShuffling of
        ConsecutiveLetters -> []
        WithAdditionalNames xs -> xs
      links = linkNames oDiagram ++ additional
  in (names, nonInheritances, links)

instance Randomise DifferentNamesInstance where
  randomise inst@DifferentNamesInstance {..} = do
    let (names, nonInheritances, lNames) = classNonInheritanceAndLinkNames inst
        links = case linkShuffling of
          ConsecutiveLetters -> take (length lNames) (map (:[]) ['z', 'y' ..])
          WithAdditionalNames _ -> lNames
    names'  <- shuffleM names
    nonInheritances' <- shuffleM nonInheritances
    links' <- shuffleM links
    renameInstance inst names' nonInheritances' links'
      >>= changeGeneratorValue
  isRandomisable DifferentNamesInstance {..} =
    isObjectDiagramRandomisable oDiagram

instance RandomiseLayout DifferentNamesInstance where
  randomiseLayout DifferentNamesInstance {..} = do
    cd <- shuffleClassAndConnectionOrder cDiagram
    od <- shuffleObjectAndLinkOrder oDiagram
    return $ DifferentNamesInstance {
      anonymousObjects = anonymousObjects,
      cDiagram = cd,
      generatorValue = generatorValue,
      oDiagram = od,
      showSolution = showSolution,
      mapping = mapping,
      linkShuffling = linkShuffling,
      usesAllRelationships = usesAllRelationships
      }

changeGeneratorValue
  :: MonadRandom m
  => DifferentNamesInstance
  -> m DifferentNamesInstance
changeGeneratorValue inst = do
  r <- getRandom
  return inst { generatorValue = r }

renameInstance
  :: MonadThrow m
  => DifferentNamesInstance
  -> [String]
  -> [String]
  -> [String]
  -> m DifferentNamesInstance
renameInstance inst names' nonInheritances' linkNs' = do
  let cd = cDiagram inst
      od = oDiagram inst
      (names, nonInheritances, linkNs) = classNonInheritanceAndLinkNames inst
      bm = BM.toAscList $ fromNameMapping $ mapping inst
      bmNames  = BM.fromList $ zip names names'
      bmNonInheritances = BM.fromList $ zip nonInheritances nonInheritances'
      bmLinks  = BM.fromList $ zip linkNs linkNs'
      bm'      = BM.fromList
        [ (a', l')
        | (a, l) <- bm
        , a' <- BM.lookup a bmNonInheritances
        , l' <- BM.lookup l bmLinks
        ]
  cd' <- renameClassesAndRelationships bmNames bmNonInheritances cd
  od' <- renameObjectsWithClassesAndLinksInOd bmNames bmLinks od
  shuffling <- mapM (`BM.lookup` bmLinks) $ linkShuffling inst
  return $ DifferentNamesInstance {
    anonymousObjects = anonymousObjects inst,
    cDiagram  = cd',
    generatorValue = generatorValue inst,
    oDiagram  = od',
    showSolution = showSolution inst,
    mapping   = toNameMapping bm',
    linkShuffling = shuffling,
    usesAllRelationships = usesAllRelationships inst
    }
