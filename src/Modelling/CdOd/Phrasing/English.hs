{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Phrasing relationships and changes in English
module Modelling.CdOd.Phrasing.English (
  phraseChange,
  phraseRelationship,
  ) where

import Modelling.Types (
  Change (..),
  )
import Modelling.CdOd.Types (
  AnyRelationship,
  InvalidRelationship (..),
  LimitedLinking (..),
  NonInheritancePhrasing (..),
  Relationship (..),
  toPhrasing,
  )

import Control.OutputCapable.Blocks     (ArticleToUse (..))
import Data.String.Interpolate          (iii)

phraseChange
  :: ArticleToUse
  -> Bool
  -> Bool
  -> Change (AnyRelationship String String)
  -> String
phraseChange article byName withDir c = case (add c, remove c) of
  (Nothing, Nothing) -> "change nothing"
  (Just e,  Nothing) -> "add " ++ phrasingNew e
  (Nothing, Just e ) -> "remove " ++ phrasingOld e
  (Just e1, Just e2) ->
    "replace " ++ phrasingOld e2
    ++ " by " ++ phrasingNew e1
  where
    phrasingOld = phraseRelation article $ toPhrasing byName withDir
    phrasingNew = phraseRelation IndefiniteArticle $ toPhrasing False withDir

consonantArticle :: ArticleToUse -> String
consonantArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "a"

vowelArticle :: ArticleToUse -> String
vowelArticle = \case
  DefiniteArticle -> "the"
  IndefiniteArticle -> "an"

phraseRelationship
  :: ArticleToUse
  -> Bool
  -> Bool
  -> AnyRelationship String String
  -> String
phraseRelationship article byName withDir = phraseRelation article phrasing
  where
    phrasing = toPhrasing byName withDir

phraseRelation
  :: ArticleToUse
  -> NonInheritancePhrasing
  -> AnyRelationship String String
  -> String
phraseRelation article = curry $ \case
  (_, Left InvalidInheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{linking invalidSubClass} inherits from #{linking invalidSuperClass}
    and #{participations invalidSubClass invalidSuperClass}
    |]
  (_, Right Inheritance {..}) -> [iii|
    #{vowelArticle article} inheritance
    where #{subClass} inherits from #{superClass}
    |]
  (ByName, Right Association {..}) -> "association " ++ associationName
  (ByName, Right Aggregation {..}) -> "aggregation " ++ aggregationName
  (ByName, Right Composition {..}) -> "composition " ++ compositionName
  (Lengthy, Right Association {..})
    | linking associationFrom == linking associationTo -> [iii|
    #{consonantArticle article} self-association for #{linking associationFrom}
    where #{participates (limits associationFrom) "it"} at one end
    and #{phraseLimit $ limits associationTo} at the other end
    |]
    | otherwise -> [iii|
    #{vowelArticle article} association
    #{participations associationFrom associationTo}
    |]
  (ByDirection, Right Association {..})
    | linking associationFrom == linking associationTo -> [iii|
    #{consonantArticle article} self-association for #{linking associationFrom}
    where #{participates (limits associationFrom) "it"} at its beginning
    and #{phraseLimit $ limits associationTo} at its arrow end
    |]
    | otherwise -> [iii|
    #{vowelArticle article} association from #{linking associationFrom}
    to #{linking associationTo}
    #{participations associationFrom associationTo}
    |]
  (_, Right Aggregation {..})
    | linking aggregationPart == linking aggregationWhole -> [iii|
    #{consonantArticle article} self-aggregation
    #{selfParticipatesPartWhole aggregationPart aggregationWhole}
    |]
    | otherwise -> [iii|
    #{consonantArticle article} relationship
    that makes #{linking aggregationWhole}
    an aggregation of #{linking aggregationPart}s
    #{participations aggregationWhole aggregationPart}
    |]
  (_, Right Composition {..})
    | linking compositionPart == linking compositionWhole -> [iii|
    #{consonantArticle article} self-composition
    #{selfParticipatesPartWhole compositionPart compositionWhole}
    |]
    | otherwise -> [iii|
    #{consonantArticle article} relationship
    that makes #{linking compositionWhole}
    a composition of #{linking compositionPart}s
    #{participations compositionWhole compositionPart}
    |]

selfParticipatesPartWhole
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
selfParticipatesPartWhole part whole = [iii|
  for #{linking part} where #{participates (limits part) "it"} as part
  and #{phraseLimit $ limits whole} as whole|]

participations
  :: LimitedLinking String
  -> LimitedLinking String
  -> String
participations from to = [iii|
  where #{participates (limits from) (linking from)}
  and #{participates (limits to) (linking to)}
  |]

participates :: (Int, Maybe Int) -> String -> String
participates r c = c ++ " participates " ++ phraseLimit r

phraseLimit :: (Int, Maybe Int) -> String
phraseLimit (0, Just 0)  = "not at all"
phraseLimit (1, Just 1)  = "exactly once"
phraseLimit (2, Just 2)  = "exactly twice"
phraseLimit (-1, Just n) = "*.." ++ show n ++ " times"
phraseLimit (m, Nothing) = show m ++ "..* times"
phraseLimit (m, Just n)  = show m ++ ".." ++ show n ++ " times"
