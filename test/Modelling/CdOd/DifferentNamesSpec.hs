{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
-- |

module Modelling.CdOd.DifferentNamesSpec where

import qualified Data.Bimap                       as BM

import Capabilities.Alloy.IO            ()
import Capabilities.Cache.IO            ()
import Capabilities.Diagrams.IO         ()
import Capabilities.Graphviz.IO         ()
import Modelling.CdOd.DifferentNames (
  DifferentNamesConfig (objectConfig),
  ShufflingOption (..),
  SolutionDisplay(..),
  differentNames,
  checkDifferentNamesConfig,
  checkDifferentNamesInstance,
  differentNamesEvaluation,
  differentNamesInitial,
  differentNamesSyntax,
  DifferentNamesInstance (..),
  defaultDifferentNamesConfig,
  defaultDifferentNamesInstance,
  defaultDifferentNamesTaskText,
  getDifferentNamesTask,
  renameInstance,
  )
import Modelling.Auxiliary.Common       (lowerFirst, oneOf)
import Modelling.CdOd.Types (
  Cd,
  ClassDiagram (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectConfig (..),
  ObjectDiagram (..),
  Od,
  Relationship (..),
  associationNames,
  classNames,
  defaultCdDrawSettings,
  linkLabels,
  normaliseObjectDiagram,
  renameObjectsWithClassesAndLinksInOd,
  )
import Modelling.Common                 (runWithoutOutput)
import Modelling.Types (
  Name (Name, unName),
  fromNameMapping,
  toNameMapping,
  )

import Control.OutputCapable.Blocks (
  ExtraText (..),
  )
import Control.Monad.Random (
  evalRandT,
  mkStdGen,
  randomIO,
  randomRIO,
  )
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Char                        (toUpper)
import Data.Containers.ListUtils        (nubOrd)
import Data.Maybe                       (fromJust, isNothing, isJust)
import Data.Ratio                       ((%))
import Data.Tuple                       (swap)
import Test.Hspec
import Test.QuickCheck (
  (==>),
  Arbitrary (arbitrary),
  NonEmptyList (NonEmpty),
  Property,
  Testable (property),
  ioProperty,
  oneof,
  sized,
  vectorOf,
  )
import System.Random                    (getStdGen, setStdGen)
import System.Random.Shuffle            (shuffleM)
import System.IO.Extra (withTempDir)

spec :: Spec
spec = do
  describe "defaultDifferentNamesConfig" $
    it "is valid" $
      checkDifferentNamesConfig defaultDifferentNamesConfig `shouldBe` Nothing
  describe "defaultDifferentNamesInstance" $ do
    it "is valid" $
      checkDifferentNamesInstance defaultDifferentNamesInstance
      `shouldBe` Nothing
    context "using WithAdditionalNames" $
      it "is valid" $
        checkDifferentNamesInstance defaultDifferentNamesInstance {
          linkShuffling = WithAdditionalNames ["v"]
          }
        `shouldBe` Nothing
  describe "differentNames" $ do
    context "using defaultDifferentNamesConfig" $ do
      it "generates an okay instance" $ do
        segment <- oneOf [0 .. 3]
        seed <- randomIO
        inst <- differentNames defaultDifferentNamesConfig segment seed
        checkDifferentNamesInstance inst `shouldBe` Nothing
      it "reproducibly generates defaultDifferentNamesInstance" $
        differentNames defaultDifferentNamesConfig 0 0
        `shouldReturn` defaultDifferentNamesInstance
  describe "differentNamesEvaluation" $ do
    it "accepts the initial example" $
      let cs = map (bimap unName unName) differentNamesInitial
      in property $ \(NonEmpty bs) -> ioProperty $
        evaluateAndCheckDifferentNames (Just 1 ==) bs cs cs
    it "accepts correct solutions" $
      property $ \(NonEmpty cs) g (NonEmpty bs) -> ioProperty $ do
          cs' <- flipCoin g `mapM` cs >>= shuffleM
          evaluateAndCheckDifferentNames (if isValidMapping cs then (Just 1 ==) else isNothing) bs cs cs'
    it "accepts with percentage or rejects too short solutions" $
      property $ \(NonEmpty cs') n (NonEmpty bs) ->
        let cs = map (\(NonEmpty x, NonEmpty y) -> (x, y)) cs'
        in isValidMapping cs ==> ioProperty $ do
          let n' = abs n
              l = fromIntegral $ length cs
              r = (l - fromIntegral n') % l
          cs'' <- drop n' <$> shuffleM cs
          evaluateAndCheckDifferentNames (if r >= 0.5 then (Just r ==) else isNothing) bs cs cs''
    it "rejects too long solutions" $
      property $ \cs (NonEmpty w) (NonEmpty bs) ->
        let cs' = cs ++ w
        in isValidMapping cs
           ==> ioProperty $ evaluateAndCheckDifferentNames isNothing bs cs cs'
  describe "renameInstance" $ do
    it "is reversible" $ renameProperty $ \inst renamedInstance _ _ ->
        let cd = cDiagram inst
            od = oDiagram inst
            names = classNames cd
            nonInheritances = associationNames cd
            linkNs = linkLabels od
        in (Just inst ==)
           $ renamedInstance
           >>= (\x -> renameInstance x names nonInheritances linkNs)
    it "renames solution" $ renameProperty $ \inst renamedInstance as ls ->
      let rename xs ys = Name . fromJust . (`lookup` zip xs ys)
          origMap = map (bimap
            (rename (associationNames $ cDiagram inst) as)
            (rename (linkLabels $ oDiagram inst) ls))
            $ BM.toList (fromNameMapping $ mapping inst)

      in ioProperty $ case maybe (Left "instance could not be renamed") return renamedInstance of
        Left _ -> pure False
        Right renamed -> do
          r <- withTempDir $ \tmpDir -> runWithoutOutput (differentNamesEvaluation tmpDir renamed origMap)
          pure $ Just 1 == r
  describe "getDifferentNamesTask" $ do
    it "generates matching OD for association circle" $
      odFor (cdSimpleCircle association association association)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for aggregation circle" $
      odFor (cdSimpleCircle aggregation aggregation aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for composition and association circle" $
      odFor (cdSimpleCircle composition composition association)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for composition and aggregation circle" $
      odFor (cdSimpleCircle composition composition aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for association and aggregation circle" $
      odFor (cdSimpleCircle association association aggregation)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for association, aggregation and composition circle" $
      odFor (cdSimpleCircle association aggregation composition)
      `shouldReturn` simpleCircleOd
    it "generates matching OD for circle with inheritance" $
      odFor cdBCCircle
      `shouldReturn` ObjectDiagram {
        objects = [
          Object {isAnonymous = True, objectName = "a", objectClass = "A"},
          Object {isAnonymous = True, objectName = "c", objectClass = "C"},
          Object {isAnonymous = True, objectName = "c1", objectClass = "C"}
          ],
        links = [
          Link {linkLabel = "x", linkFrom = "a", linkTo = "c"},
          Link {linkLabel = "x", linkFrom = "a", linkTo = "c1"},
          Link {linkLabel = "y", linkFrom = "c", linkTo = "a"},
          Link {linkLabel = "y", linkFrom = "c1", linkTo = "a"}
          ]
        }

odFor :: Cd -> IO Od
odFor cd = normaliseObjectDiagram <$> do
  g <- getStdGen
  evalRandT (getDifferentNamesTask failed fewObjects cd) g
    >>= getOriginalOd
  where
    names = classNames cd
    keepClassNames = BM.fromList $ zip names names
    getOriginalOd x =
      renameObjectsWithClassesAndLinksInOd
      keepClassNames
      (BM.twist $ fromNameMapping $ mapping x)
      $ oDiagram x
    failed = error "failed generating instance"
    fewObjects = defaultDifferentNamesConfig { objectConfig = oc }
    oc = ObjectConfig {
      linkLimits = (0, Just 4),
      linksPerObjectLimits = (0, Just 4),
      objectLimits = (3, 3)
      }

cdBCCircle :: Cd
cdBCCircle = ClassDiagram {
  classNames = ["A", "B", "C"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    Aggregation {
      aggregationName = "x",
      aggregationPart = LimitedLinking {
        linking = "B",
        limits = (1, Just 1)
        },
      aggregationWhole = LimitedLinking {
        linking = "C",
        limits = (2, Just 2)
        }
       },
    Association {
      associationName = "y",
      associationFrom = LimitedLinking {
        linking = "C",
        limits = (0, Nothing)
        },
      associationTo = LimitedLinking {
        linking = "A",
        limits = (1, Just 1)
        }
       }
    ]
  }

type ToRelationship = String -> String -> String -> Relationship String String

cdSimpleCircle
  :: ToRelationship
  -> ToRelationship
  -> ToRelationship
  -> Cd
cdSimpleCircle edgeX edgeY edgeZ = ClassDiagram {
  classNames = ["A", "B", "C"],
  relationships = [edgeX "x" "A" "B", edgeY "y" "B" "C", edgeZ "z" "C" "A"]
  }

lcOne :: nodeName -> LimitedLinking nodeName
lcOne c = LimitedLinking {linking = c, limits = one}
  where
    one = (1, Just 1)

association :: r -> c -> c -> Relationship c r
association name from to= Association {
  associationName = name,
  associationFrom = lcOne from,
  associationTo = lcOne to
  }

aggregation :: r -> c -> c -> Relationship c r
aggregation name part whole = Aggregation {
  aggregationName = name,
  aggregationPart = lcOne part,
  aggregationWhole = lcOne whole
  }

composition :: r -> c -> c -> Relationship c r
composition name part whole = Composition {
  compositionName = name,
  compositionPart = lcOne part,
  compositionWhole = lcOne whole
  }

simpleCircleOd :: Od
simpleCircleOd = ObjectDiagram {
  objects = [
    Object {isAnonymous = True, objectName = "a", objectClass = "A"},
    Object {isAnonymous = True, objectName = "b", objectClass = "B"},
    Object {isAnonymous = True, objectName = "c", objectClass = "C"}
    ],
  links = [
    Link {linkLabel = "x", linkFrom = "a", linkTo = "b"},
    Link {linkLabel = "y", linkFrom = "b", linkTo = "c"},
    Link {linkLabel = "z", linkFrom = "c", linkTo = "a"}
    ]
  }

renameProperty ::
  Testable prop =>
  (DifferentNamesInstance
    -> Maybe DifferentNamesInstance
    -> [String]
    -> [String]
    -> prop)
  -> Property
renameProperty p = property $ \n1 n2 n3 n4 a1 a2 a3 l1 l2 l3 ->
  let inst = defaultDifferentNamesInstance
      ns = map unName [n1, n2, n3, n4]
      as = map unName [a1, a2, a3]
      ls = map unName [l1, l2, l3]
      distinct xs = length (nubOrd xs) == length xs
      renamedInstance = renameInstance inst ns as ls
  in distinct (map lowerFirst ns) && distinct (ns ++ as ++ ls)
     ==> p inst renamedInstance as ls

instance Arbitrary Name where
  arbitrary = sized $ \s -> Name <$> (
    (:)
    <$> oneof (map return letters)
    <*> vectorOf s (oneof $ map return $ letters ++ ['0'..'9'])
    )
    where
      lowers = ['a'..'z']
      uppers = map toUpper lowers
      letters = lowers ++ uppers

flipCoin :: Int -> (String, String) -> IO (String, String)
flipCoin g p = do
  setStdGen $ mkStdGen g
  b <- randomRIO (False, True)
  return $ (if b then swap else id) p

evaluateAndCheckDifferentNames
  :: (Maybe Rational -> Bool)
  -- ^ result predicate
  -> [Bool]
  -- ^ random distribution (must not be empty)
  -> [(String, String)]
  -- ^ task instance mapping
  -> [(String, String)]
  -- ^ submitted mapping
  -> IO Bool
evaluateAndCheckDifferentNames check coins cs cs' = do
  let i = DifferentNamesInstance {
        cdDrawSettings = defaultCdDrawSettings,
        cDiagram = ClassDiagram {
          classNames = [classA],
          relationships = map newAssociation associationsToUse
          },
        oDiagram = ObjectDiagram {
          objects = [Object False linkA classA],
          links = map newLink linksToUse
          },
        showSolution = ShowMapping,
        mapping = toNameMapping $ BM.fromList cs,
        linkShuffling = ConsecutiveNumbers,
        taskText = defaultDifferentNamesTaskText,
        addText = NoExtraText
        }
      cs'' = map (bimap Name Name) cs'
  synResult <- runWithoutOutput $ differentNamesSyntax i cs''
  semResult <- if isJust synResult
    then withTempDir $ \tmpDir -> runWithoutOutput $ differentNamesEvaluation tmpDir i cs''
    else pure Nothing

  pure $ check semResult
  where
    linkA = "a"
    classA = "A"
    (associationsToUse, linksToUse) =
      unzip $ zipWith (\case True -> swap; False -> id) (cycle coins) cs
    newAssociation x = Association
      x
      (LimitedLinking classA (0, Just 1))
      (LimitedLinking classA (0, Just 1))
    newLink x = Link x linkA linkA

isValidMapping :: Ord a => [(a, a)] -> Bool
isValidMapping cs
  | 2 * length cs > length (nubOrd $ map fst cs ++ map snd cs)
  = False
  | otherwise
  = True
