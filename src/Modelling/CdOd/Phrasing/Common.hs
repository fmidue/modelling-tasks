{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Common phrasing logic for CdOd tasks
module Modelling.CdOd.Phrasing.Common (
  PhrasingStrings (..),
  phraseChangeWith,
  englishStrings,
  germanStrings
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

-- | Language-specific strings for phrasing
data PhrasingStrings = PhrasingStrings
  { changeNothing :: String
  , addPrefix :: String
  , removePrefix :: String
  , replacePrefix :: String
  , byInfix :: String
  , postProcess :: String -> String  -- For things like trailing commas
  , phraseRelationFn :: OmittedDefaultMultiplicities
                     -> ArticleToUse
                     -> PhrasingKind
                     -> NonInheritancePhrasing
                     -> AnyRelationship String String
                     -> String
  }

-- | English phrasing strings
englishStrings :: (OmittedDefaultMultiplicities -> ArticleToUse -> PhrasingKind -> NonInheritancePhrasing -> AnyRelationship String String -> String) -> PhrasingStrings
englishStrings phraseRelationFn = PhrasingStrings
  { changeNothing = "change nothing"
  , addPrefix = "add "
  , removePrefix = "remove "
  , replacePrefix = "replace "
  , byInfix = " by "
  , postProcess = id
  , phraseRelationFn = phraseRelationFn
  }

-- | German phrasing strings
germanStrings :: (OmittedDefaultMultiplicities -> ArticleToUse -> PhrasingKind -> NonInheritancePhrasing -> AnyRelationship String String -> String) -> PhrasingStrings
germanStrings phraseRelationFn = PhrasingStrings
  { changeNothing = "verändere nichts"
  , addPrefix = "ergänze "
  , removePrefix = "entferne "
  , replacePrefix = "ersetze "
  , byInfix = " durch "
  , postProcess = \xs -> if ',' `elem` xs then xs ++ "," else xs
  , phraseRelationFn = phraseRelationFn
  }

-- | Common change phrasing logic parameterized by language strings
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
    phrasingOld = phraseRelationFn strings
      defaultMultiplicities
      article
      Denoted
      $ toPhrasing byName withDir
    phrasingNew = phraseRelationFn strings
      defaultMultiplicities
      IndefiniteArticle
      Participations
      $ toPhrasing False withDir