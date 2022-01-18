{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings, OverloadedLists #-}

module Modelling.PetriNet.Reach.Group (groupSVG) where

import Text.XML.HXT.Core
    ( returnA,
      (>>>),
      readString,
      runX,
      no,
      withInputEncoding,
      withMimeTypeFile,
      withRemoveWS,
      withStrictInput,
      withValidate,
      yes,
      isoLatin1,
      ArrowList(listA),
      ArrowTree(deep),
      ArrowXml(getAttrValue, isElem, hasName),
      IOStateArrow,
      XNode,
      XmlTree )

import Data.Tree.NTree.TypeDefs (NTree)

import qualified Data.Text.Lazy.IO as Te
import qualified Text.XML as XML

import qualified Data.Text as T (Text, pack, filter, length)
import Data.List (isPrefixOf)

data SVGOptions = SVGOptions
  { xmlns, height, iStrokeOpacity, viewBox, fontSize, width, xmlnsXlink, iStroke, version :: T.Text,
    groups :: [SVGGroup] }
  deriving (Show, Eq)

data SVGGroup = SVGGroup
  { strokeLinejoin, strokeOpacity, fillOpacity, stroke, strokeWidth, fill, strokeLinecap, strokeMiterlimit, svgClass :: T.Text,
    paths :: [Path] }
  deriving (Show, Eq)

data Path = Path
  { d, pClass, pFillOpacity :: T.Text }
  deriving (Show, Eq)

parseXML :: String -> IOStateArrow s b XmlTree
parseXML = readString [ withInputEncoding  isoLatin1
                        , withValidate       no
                        , withMimeTypeFile   "/etc/mime.types"
                        , withStrictInput    no
                        , withRemoveWS       yes
                        ]

atTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

getGroup :: ArrowXml cat => cat (NTree XNode) SVGGroup
getGroup = atTag "g" >>>
  proc g -> do
    sljoin      <- getAttrValue "stroke-linejoin"   -< g
    sopacity    <- getAttrValue "stroke-opacity"    -< g
    fopacity    <- getAttrValue "fill-opacity"      -< g
    stro        <- getAttrValue "stroke"            -< g
    strowidth   <- getAttrValue "stroke-width"      -< g
    fll         <- getAttrValue "fill"              -< g
    slinecap    <- getAttrValue "stroke-linecap"    -< g
    smiterlimit <- getAttrValue "stroke-miterlimit" -< g
    gsvgClass   <- getAttrValue "class"             -< g
    gpaths      <- listA getPath                    -< g
    returnA -< SVGGroup
      { strokeLinejoin = T.pack sljoin,
        strokeOpacity  = T.pack sopacity,
        fillOpacity  = T.pack fopacity,
        stroke  = T.pack stro,
        strokeWidth  = T.pack strowidth,
        fill  = T.pack fll,
        strokeLinecap  = T.pack slinecap,
        strokeMiterlimit  = T.pack smiterlimit,
        svgClass = T.pack gsvgClass,
        paths = gpaths }

getPath :: ArrowXml cat => cat (NTree XNode) Path
getPath = atTag "path" >>>
  proc p -> do
    dAtt <- getAttrValue "d" -< p
    returnA -< Path
      { d = T.pack dAtt,
        pClass = "default",
        pFillOpacity = "0" }

getSVGAttributes :: ArrowXml cat => cat (NTree XNode) SVGOptions
getSVGAttributes = atTag "svg" >>>
  proc s -> do
    sxmlns         <- getAttrValue "xmlns"          -< s
    sheight        <- getAttrValue "height"         -< s
    sstrokeOpacity <- getAttrValue "stroke-opacity" -< s
    sviewBox       <- getAttrValue "viewBox"        -< s
    sfontSize      <- getAttrValue "font-size"      -< s
    swidth         <- getAttrValue "width"          -< s
    sxmlnsXlink    <- getAttrValue "xmlns:xlink"    -< s
    sstroke        <- getAttrValue "stroke"         -< s
    sversion       <- getAttrValue "version"        -< s
    sgroups        <- listA getGroup -< s
    returnA -< SVGOptions
      {
          xmlns = T.pack sxmlns,
          height = T.pack sheight,
          iStrokeOpacity = T.pack sstrokeOpacity,
          viewBox = T.pack sviewBox,
          fontSize = T.pack sfontSize,
          width = T.pack swidth,
          xmlnsXlink = T.pack sxmlnsXlink,
          iStroke = T.pack sstroke,
          version = T.pack sversion,
          groups = sgroups
      }

applyClass :: SVGGroup -> [Path]
applyClass x = [ p{ pClass = T.filter (/='.') (svgClass x), pFillOpacity = fillOpacity x } | p <- paths x]

formatSVG :: [SVGGroup] -> [SVGGroup] -> [SVGGroup]
formatSVG [] ys = ys
formatSVG (x:xs) ys = formatSVG rest ((x{ paths = gpaths }):ys)
                where
                  gpaths 
                    | isLabelOrEdge x = concatMap applyClass (filter (equalGroup (svgClass x) . svgClass) (x:xs))
                    | otherwise = applyClass x ++ concatMap applyClass (takeWhile (equalGroup (svgClass x) . svgClass) xs)
                  rest
                    | isLabelOrEdge x = filter (not . equalGroup (svgClass x) . svgClass) xs
                    | otherwise = dropWhile (equalGroup (svgClass x) . svgClass) xs
                  isLabelOrEdge z = let fx = T.filter (/='.') (svgClass z) in fx == "elabel" || fx == "edge"

equalGroup :: T.Text -> T.Text -> Bool
equalGroup x y
  | (fx == "edge" || fx == "elabel") && (fy == "elabel" || fy == "edge") && sameLength = True
  | (fx == "node") && (fy == "token" || fy == "nlabel") = True
  | (fx == "rect") && (fy == "") = True
  | otherwise = False
  where fx = T.filter (/= '.') x
        fy = T.filter (/= '.') y
        sameLength = T.length (T.filter (== '.') x) == T.length (T.filter (== '.') y)

buildSVG :: SVGOptions -> XML.Element
buildSVG svg = XML.Element "svg" [("xmlns", xmlns svg), ("height", height svg), ("stroke-opacity", iStrokeOpacity svg), ("viewBox", viewBox svg), ("font-size", fontSize svg), ("width", width svg), ("xmlns:xlink", xmlnsXlink svg), ("stroke", iStroke svg), ("version", version svg)]
  [ XML.NodeElement $ XML.Element "g" [("stroke-linejoin", strokeLinejoin gr), ("stroke-opacity", strokeOpacity gr), ("fill-opacity", fillOpacity gr), ("stroke", stroke gr), ("stroke-width", strokeWidth gr), ("fill", fill gr), ("stroke-linecap", strokeLinecap gr), ("stroke-miterlimit", strokeMiterlimit gr)]
   [ XML.NodeElement $ XML.Element "path" [("d", d ps), ("class", pClass ps), ("fill-opacity", pFillOpacity ps)] [] | ps <- paths gr] | gr <- groups svg ]

removeDoctype :: Bool -> String -> String
removeDoctype _ "" = ""
removeDoctype False s@(c:cs) 
  | "<!DOCTYPE" `isPrefixOf` s = removeDoctype True (drop 9 s)
  | otherwise = c : removeDoctype False cs
removeDoctype True (c:cs)
  | c == '>' = cs
  | otherwise = removeDoctype True cs

groupSVG :: FilePath -> IO ()
groupSVG svgPath = do
  s <- readFile svgPath
  (x:_) <- runX (parseXML (removeDoctype False s) >>> getSVGAttributes)
  Te.writeFile svgPath (XML.renderText XML.def $ XML.Document (XML.Prologue [] Nothing []) (buildSVG x{groups = formatSVG (groups x) []}) [])