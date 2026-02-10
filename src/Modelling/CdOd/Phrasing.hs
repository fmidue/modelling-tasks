{-# LANGUAGE LambdaCase #-}
-- | provide phrasing functions for different languages
module Modelling.CdOd.Phrasing (
  phraseChange,
  phraseRelationship,
  trailingCommaGerman,
  numberToWord,
  ) where

import qualified Modelling.CdOd.Phrasing.German    as German
import qualified Modelling.CdOd.Phrasing.English   as English

import qualified Data.Map as M (lookup)

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

numberToWord :: Int -> Language -> Maybe String
numberToWord n lang = M.lookup n numberWords
  where
    numberWords = case lang of
      English -> English.numberWords
      German  -> German.numberWords

trailingCommaGerman :: String -> String
trailingCommaGerman = German.trailingComma
