{-# LANGUAGE RecordWildCards #-}
module Modelling.CdOd.Auxiliary.Util (
  alloyInstanceToOd,
  emptyArr,
  filterFirst,
  redColor, underlinedLabel,
  ) where

import qualified Data.Set                         as S

import Modelling.Auxiliary.Common       (lowerFirst)
import Modelling.CdOd.Types (
  Link (..),
  Object (..),
  ObjectDiagram (..),
  Od,
  )

import Language.Alloy.Call              as Alloy (
  AlloyInstance,
  getSingleAs,
  getTripleAs,
  lookupSig,
  scoped,
  )

import Control.Monad.Catch              (MonadThrow)
import Data.GraphViz                    (X11Color (..))
import Data.GraphViz.Attributes.Complete (
  ArrowShape (..),
  ArrowType (..),
  Attribute (..),
  Label (HtmlLabel),
  openMod,
  toWColor,
  )
import Data.GraphViz.Attributes.HTML    as Html
  (Label, Format (..), Label (Text), TextItem (..))
import Data.Text.Lazy                   (pack)

filterFirst :: Eq a => a -> [a] -> [a]
filterFirst _ []     = []
filterFirst x (y:ys) = if x == y then ys else y : filterFirst x ys

underlinedLabel :: String -> Attribute
underlinedLabel s = Label (HtmlLabel label)
  where
    label :: Html.Label
    label = Text [Format Underline [Str (pack s)]]

emptyArr :: ArrowType
emptyArr = AType [(openMod, Normal)]

redColor :: Attribute
redColor = Color [toWColor Red]

alloyInstanceToOd
  :: MonadThrow m
  => AlloyInstance
  -> m Od
alloyInstanceToOd i = do
  os    <- lookupSig (scoped "this" "Object") i
  objects <- S.toList <$> getSingleAs "" toObject os
  links <- fmap toLink . S.toList <$> getTripleAs "get" oName nameOnly oName os
  return ObjectDiagram {..}
  where
    oName x = return . toObjectName x
    toObjectName x y = lowerFirst x ++ if y == 0 then [] else show y
    toObject x y = return $ Object {
      objectName = toObjectName x y,
      objectClass = x
      }
    nameOnly x _ = return x
    toLink (x, l, y) = Link {
      linkName = l,
      linkFrom = x,
      linkTo = y
      }
