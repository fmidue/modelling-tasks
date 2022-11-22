{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Modelling.CdOd.Output (
  drawCdFromSyntax,
  drawOdFromInstance,
  drawOdFromRawInstance,
  drawOdFromNodesAndEdges,
  getDirs,
  ) where

import qualified Data.Map                         as M (
  empty,
  foldrWithKey,
  insert,
  lookup,
  )
import qualified Diagrams.TwoD.GraphViz           as GV (getGraph)

import Modelling.Auxiliary.Common       (lowerFirst)
import Modelling.Auxiliary.Diagrams (
  arrowheadDiamond,
  arrowheadFilledDiamond,
  arrowheadTriangle,
  arrowheadVee,
  connectWithPath,
  flipArrow,
  text',
  textU,
  varrow,
  )
import Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  emptyArr,
  underlinedLabel,
  )
import Modelling.CdOd.Types
  (AssociationType(..), Connection(..), DiagramEdge, Syntax)
import Modelling.CdOd.Edges             (shouldBeMarked)
import Modelling.PetriNet.Reach.Group   (writeSVG)

import Control.Lens                     ((.~))
import Control.Monad.Random             (RandT, RandomGen)
import Control.Monad.Trans              (MonadTrans(lift))
import Control.Monad.Trans.Except       (runExcept)
import Data.Graph.Inductive             (Gr, mkGraph)
import Data.GraphViz (
  DirType (Back, Forward, NoDir),
  GraphvizParams (..),
  Shape (BoxShape),
  arrowFrom,
  arrowTo,
  diamond,
  dirCommand,
  edgeEnds,
  noArrow,
  nonClusteredParams,
  oDiamond,
  quitWithoutGraphviz,
  shape,
  toLabel,
  toLabelValue,
  undirCommand,
  vee,
  )
import Data.GraphViz.Attributes.Complete (Attribute (..), DPoint (..), Label)
import Data.Function                    ((&))
import Data.List (
  elemIndex, intercalate, isPrefixOf, stripPrefix,
  )
import Data.List.Split                  (splitOn)
import Data.Map                         (Map)
import Data.Maybe                       (fromJust, fromMaybe, maybeToList)
import Data.String.Interpolate          (iii)
import Diagrams.Align                   (center)
import Diagrams.Angle                   ((@@), deg)
import Diagrams.Attributes              (lineWidth, lwL)
import Diagrams.Backend.SVG             (B)
import Diagrams.Combinators             (atop, frame)
import Diagrams.Names                   (IsName, named)
import Diagrams.Path                    (Path, reversePath)
import Diagrams.Points                  (Point(..))
import Diagrams.Prelude (
  Diagram,
  Style,
  applyStyle,
  black,
  local,
  white,
  )
import Diagrams.Transform               (translate)
import Diagrams.TwoD                    (V2, bg, snugCenterXY)
import Diagrams.TwoD.Arrow (
  arrowHead,
  arrowTail,
  headGap,
  headLength,
  tailLength,
  )
import Diagrams.TwoD.Arrowheads         (lineTail)
import Diagrams.TwoD.Attributes         (fc, lc)
import Diagrams.TwoD.GraphViz           (layoutGraph')
import Diagrams.Util                    ((#), with)
import Graphics.SVGFonts.Fonts          (lin)
import Graphics.SVGFonts.ReadFont       (PreparedFont)
import Language.Alloy.Call              (AlloyInstance)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Random.Shuffle            (shuffleM)

debug :: Bool
debug = False

connectionArrow :: Bool -> Bool -> Maybe Attribute -> Connection -> [Attribute]
connectionArrow _ _ _ Inheritance =
  [arrowTo emptyArr]
connectionArrow _ printNames marking (Assoc Composition name from to isMarked) =
  arrow Composition ++ [HeadLabel (mult to)]
  ++ concat [maybeToList marking | isMarked] ++ [toLabel name | printNames]
  ++ case from of
       (1, Just 1) -> []
       (0, Just 1) -> [TailLabel (mult from)]
       (0, Nothing)-> [TailLabel $ toLabelValue "0..*"]
       (_, _)      -> (
         if debug
         then \x -> unsafePerformIO $ do
           putStrLn "invalid composition multiplicity"
           return x
         else id
         )
         [TailLabel (mult from)]
connectionArrow printNavigations printNames marking (Assoc a name from to isMarked) =
  printArrow a
  ++ [TailLabel (mult from), HeadLabel (mult to)]
  ++ concat [maybeToList marking | isMarked] ++ [toLabel name | printNames]
  where
    printArrow
      | printNavigations = arrowDirected
      | otherwise        = arrow

arrowDirected :: AssociationType -> [Attribute]
arrowDirected Association = [arrowTo vee, ArrowSize 0.4]
arrowDirected a           = arrow a

arrow :: AssociationType -> [Attribute]
arrow Association = [ArrowHead noArrow]
arrow Aggregation = [arrowFrom oDiamond, edgeEnds Back]
arrow Composition = [arrowFrom diamond, edgeEnds Back]

mult :: (Int, Maybe Int) -> Label
mult (-1, Just u) = toLabelValue $ "*.." ++ show u
mult (0, Nothing) = toLabelValue ""
mult (l, Nothing) = toLabelValue (show l ++ "..*")
mult (l, Just u) | l == u    = toLabelValue l
                 | otherwise = toLabelValue (show l ++ ".." ++ show u)

drawCdFromSyntax
  :: Bool
  -> Bool
  -> Style V2 Double
  -> Syntax
  -> FilePath
  -> IO FilePath
drawCdFromSyntax printNavigations printNames marking syntax file = do
  let (classes, associations) = syntax
  let classNames = map fst classes
  let theNodes = classNames
  let inhEdges = [( fromJust (elemIndex from theNodes)
                  , fromJust (elemIndex to theNodes)
                  , Inheritance)
                 | (from, tos) <- classes, to <- tos]
  let classesWithSubclasses = map (\name -> (name, subs [] name)) classNames
        where
          subs seen name
            | name `elem` seen = []
            | otherwise = name : concatMap
                (subs (name:seen) . fst)
                (filter ((name `elem`) . snd) classes)
  let assocsBothWays = concatMap (\(_,_,_,from,to,_) -> [(from,to), (to,from)]) associations
  let assocEdges = map (
        \(a,n,m1,from,to,m2) -> (
          fromJust (elemIndex from theNodes),
          fromJust (elemIndex to theNodes),
          Assoc a n m1 m2 (shouldBeMarked from to classesWithSubclasses assocsBothWays)
          )
        ) associations
  let graph = mkGraph (zip [0..] theNodes) (inhEdges ++ assocEdges)
        :: Gr String Connection
  let params = nonClusteredParams {
        fmtNode = \(_,l) -> [
          toLabel l,
          shape BoxShape,
          Margin $ DVal 0.02,
          Width 0,
          Height 0,
          FontSize 16
          ],
        fmtEdge = \(_,_,l) -> FontSize 16
          : connectionArrow printNavigations printNames Nothing l
        }
  errorWithoutGraphviz
  graph' <- layoutGraph' params dirCommand graph
  sfont  <- lin
  let (nodes, edges) = GV.getGraph graph'
      gnodes = M.foldrWithKey
        (\l p g -> drawClass sfont l p `atop` g)
        mempty
        nodes
      gedges = foldr
        (\(s, t, l, p) g -> g # drawRel sfont s t l p)
        gnodes
        edges
  let file' = file ++ ".svg"
  writeSVG file' gedges
  return file'
  where
    drawRel f = drawRelationship f printNavigations printNames marking

drawRelationship
  :: IsName n
  => PreparedFont Double
  -> Bool
  -> Bool
  -> (Style V2 Double)
  -> n
  -> n
  -> Connection
  -> Path V2 Double
  -> Diagram B
  -> Diagram B
drawRelationship sfont printNavigations printNames marking fl tl l path g =
  connectWithPath opts sfont dir from to ml mfl mtl path' g
  # applyStyle (if isMarked then marking else mempty)
  # lwL 0.5
  where
    angle :: Double
    angle = 150
    opts = with
      & arrowTail .~ atail (angle @@ deg)
      & arrowHead .~ ahead (angle @@ deg)
      & headLength .~ local 7
      & headGap .~ local 0
      & tailLength .~ local 7
    mfl' = case l of
      Inheritance -> Nothing
      Assoc Composition _ r _ _ -> rangeWithDefault (1, Just 1) r
      Assoc _ _ r _ _ -> rangeWithDefault (0, Nothing) r
    mtl' = case  l of
      Inheritance -> Nothing
      Assoc _ _ _ r _ -> rangeWithDefault (0, Nothing) r
    (from, to, mfl, mtl, path')
      | flipEdge  = (tl, fl, mtl', mfl', reversePath path)
      | otherwise = (fl, tl, mfl', mtl', path)
    atail = const lineTail
    (flipEdge, ahead) = case l of
      Inheritance -> (False, arrowheadTriangle)
      Assoc t _ _ _ _ -> case t of
        Association -> (
          False,
          if printNavigations then arrowheadVee else const (flipArrow lineTail)
          )
        Aggregation -> (True, arrowheadDiamond)
        Composition -> (True, arrowheadFilledDiamond)
    dir = case l of
      Assoc Association _ _ _ _ ->
        if printNavigations then Forward else NoDir
      _ -> Forward
    (ml, isMarked) = case l of
      Inheritance      -> (Nothing, False)
      Assoc _ al _ _ im -> (,im) $
        if printNames then Just al else Nothing

rangeWithDefault :: (Int, Maybe Int) -> (Int, Maybe Int) -> Maybe String
rangeWithDefault def fromTo
  | def == fromTo = Nothing
  | otherwise     = Just $ range fromTo
  where
    range (l, Nothing) = show l ++ "..*"
    range (l, Just u)
      | l == -1   = "*.." ++ show u
      | l == u    = show l
      | otherwise = show l ++ ".." ++ show u

drawClass
  :: PreparedFont Double
  -> String
  -> Point V2 Double
  -> Diagram B
drawClass sfont l (P p) = translate p
  $ center $ blackFrame l $ center
  $ text' sfont 16 l
  # snugCenterXY
  # lineWidth 0.6

errorWithoutGraphviz :: IO ()
errorWithoutGraphviz =
  quitWithoutGraphviz [iii|
    Please install GraphViz executables from http://graphviz.org/
    and put them on your PATH
    |]

drawOdFromInstance
  :: RandomGen g
  => AlloyInstance
  -> Maybe Int
  -> Map String DirType
  -> Bool
  -> FilePath
  -> RandT g IO FilePath
drawOdFromInstance i anonymous =
  let g = either error id $ runExcept $ alloyInstanceToOd i
  in uncurry drawOdFromNodesAndEdges g $ fromMaybe (length (fst g) `div` 3) anonymous

drawOdFromRawInstance
  :: RandomGen g
  => String
  -> Map String DirType
  -> Bool
  -> FilePath
  -> RandT g IO FilePath
drawOdFromRawInstance input =
  let [objLine, objGetLine] = filter ("this/Obj" `isPrefixOf`) (lines input)
      theNodes = splitOn ", " (init (tail (fromJust (stripPrefix "this/Obj=" objLine))))
      theEdges = map ((\[from,v,to] -> (
                          fromJust (elemIndex from theNodes),
                          fromJust (elemIndex to theNodes),
                          takeWhile (/= '$') v)) . splitOn "->"
                     )
                 $ filter (not . null)
                 $ splitOn ", "
                 $ init $ tail $ fromJust
                 $ stripPrefix "this/Obj<:get=" objGetLine
  in drawOdFromNodesAndEdges theNodes theEdges (length theNodes `div` 3)

drawOdFromNodesAndEdges
  :: RandomGen g
  => [String]
  -> [(Int, Int, String)]
  -> Int
  -> Map String DirType
  -> Bool
  -> FilePath
  -> RandT g IO FilePath
drawOdFromNodesAndEdges theNodes theEdges anonymous navigations printNames file = do
  let numberedNodes = zip [0..] theNodes
  let graph = mkGraph numberedNodes theEdges :: Gr String String
  objectNames <-
    map (\(i, l) -> (i, removeDollar l ++ " "))
    . drop anonymous
    <$> shuffleM numberedNodes
  let params = nonClusteredParams {
        fmtNode = \(i,l) -> [
          underlinedLabel $ fromMaybe "" (lookup i objectNames)
          ++ ": " ++ takeWhile (/= '$') l,
          shape BoxShape,
          Margin $ DVal 0.02,
          Width 0,
          Height 0,
          FontSize 16
          ],
        fmtEdge = \(_,_,l) -> arrowHeads l
          ++ [ArrowSize 0.4, FontSize 16]
          ++ [toLabel l | printNames] }
  let objectNames' = (\(i, n) -> (fromMaybe "" $ lookup i numberedNodes, n)) <$> objectNames
  lift errorWithoutGraphviz
  graph' <- lift $ layoutGraph' params undirCommand graph
  sfont  <- lift lin
  let (nodes, edges) = GV.getGraph graph'
      gnodes = M.foldrWithKey
        (\l p g -> drawObject sfont objectNames' l p `atop` g)
        mempty
        nodes
      gedges = foldr
        (\(s, t, l, p) g -> g # drawLink sfont navigations printNames s t l p)
        gnodes
        edges
  let file' = file ++ ".svg"
  lift $ writeSVG file' gedges
  return file'
  where
    removeDollar l = case splitOn "$" l of
      n:xs@(_:_) ->
        let z  = last xs
            ys = intercalate "$" $ init xs
        in lowerFirst n ++ ys ++ (if z == "0" then "" else z)
      _          -> l
    arrowHeads l = case M.lookup l navigations of
      Nothing  -> [edgeEnds NoDir]
      Just dir -> [edgeEnds dir, arrowFrom vee, arrowTo vee]

drawLink
  :: (IsName n1, IsName n2)
  => PreparedFont Double
  -> Map String DirType
  -> Bool
  -> n1
  -> n2
  -> String
  -> Path V2 Double
  -> Diagram B
  -> Diagram B
drawLink sfont navigations printNames fl tl l =
  connectWithPath opts sfont dir fl tl ml Nothing Nothing
  # lwL 0.5
  where
    opts = with
      & arrowTail .~ lineTail
      & arrowHead .~ varrow
      & headLength .~ local 7
      & headGap .~ local 0
      & tailLength .~ local 7
    ml
      | printNames = Just l
      | otherwise  = Nothing
    dir = fromMaybe NoDir $ M.lookup l navigations

drawObject
  :: PreparedFont Double
  -> [(String, String)]
  -> String
  -> Point V2 Double
  -> Diagram B
drawObject sfont objectNames t (P p) = translate p
  $ center $ blackFrame t $ center
  $ textU sfont 16
      (fromMaybe "" (lookup t objectNames) ++ ": " ++ takeWhile (/= '$') t)
  # snugCenterXY
  # lineWidth 0.6

blackFrame
  :: String
  -> Diagram B
  -> Diagram B
blackFrame t object =
  frame 1 (frame 2 object
            # fc black
            # lc black
            # bg white)
  # bg black
  # named t

getDirs :: [DiagramEdge] -> Map String DirType
getDirs es =
  let backwards   = [n | (_, _, Assoc t n _ _ _) <-es
                       , t /= Association]
      forwards    = [n | (_, _, Assoc t n _ _ _) <- es
                       , t == Association]
   in foldr (`M.insert` Back)
            (foldr (`M.insert` Forward) M.empty forwards)
             backwards
