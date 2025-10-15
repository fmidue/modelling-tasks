{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | This module provides basic types

module Modelling.Types (
  Change (..),
  Letters (..),
  Name (..),
  NameMapping (..),
  fromNameMapping,
  parseLettersPrec,
  parseNamePrec,
  showLetters,
  showName,
  toNameMapping,
  ) where

import qualified Data.Bimap                       as BM

import Modelling.Auxiliary.Common       (skipSpaces)

import Data.Bimap                       (Bimap)
import Data.Char                        (isAlpha, isAlphaNum)
import Data.String                      (IsString (fromString))
import GHC.Generics                     (Generic)
import Text.ParserCombinators.Parsec (
  Parser,
  many1,
  optional,
  satisfy,
  endBy,
  )

newtype Name = Name { unName :: String }
  deriving (Eq, Generic, Ord, Show)

-- | Custom Read instance for Name that accepts optional trailing periods.
-- This allows parsing both bare alphanumeric names (e.g., "1", "abc")
-- and names with trailing periods (e.g., "1.", "abc.").
-- The period is stripped during parsing.
-- Also supports standard constructor syntax: Name "1" and Name "1."
instance Read Name where
  readsPrec prec input =
    -- Try to read with Name constructor and a string argument
    readParen (prec > 10) (\s -> do
      ("Name", s1) <- lex s
      (str, s2) <- reads s1  -- reads a String, handles quotes and escapes
      let nameStr = stripTrailingPeriod str
      return (Name nameStr, s2)) input
    -- Also try to read just alphanumeric chars with optional period (for bare usage)
    ++ readBareAlphaNum input
    where
      stripTrailingPeriod :: String -> String
      stripTrailingPeriod [] = []
      stripTrailingPeriod str =
        case reverse str of
          ('.':rest) -> reverse rest
          _ -> str

      readBareAlphaNum str =
        let trimmed = dropWhile (== ' ') str
            (alphanumericName, rest1) = span isAlphaNum trimmed
            rest2 = case rest1 of
              ('.':xs) -> xs
              xs -> xs
        in [(Name alphanumericName, rest2) | not (null alphanumericName)]

instance IsString Name where
  fromString = Name

showName :: Name -> String
showName = unName

parseNamePrec :: Int -> Parser Name
parseNamePrec _ = do
  skipSpaces
  name <- many1 (satisfy isAlphaNum)
  _ <- optional (satisfy (== '.'))
  skipSpaces
  return $ Name name

newtype Letters = Letters { lettersList :: String }
  deriving (Eq, Generic, Ord, Read, Show)

instance IsString Letters where
  fromString = Letters

showLetters :: Letters -> String
showLetters = lettersList

parseLettersPrec :: Int -> Parser Letters
parseLettersPrec _ = do
  skipSpaces
  Letters <$> endBy (satisfy isAlpha) skipSpaces

newtype NameMapping = NameMapping { nameMapping :: Bimap Name Name }
  deriving (Eq, Generic)

fromNameMapping :: NameMapping -> Bimap String String
fromNameMapping = BM.mapMonotonic unName . BM.mapMonotonicR unName . nameMapping

toNameMapping :: Bimap String String -> NameMapping
toNameMapping = NameMapping . BM.mapMonotonic Name . BM.mapMonotonicR Name

instance Show NameMapping where
  show = show . BM.toList . nameMapping

instance Read NameMapping where
  readsPrec p xs = [(NameMapping $ BM.fromList y, ys) | (y, ys) <- readsPrec p xs]

data Change a = Change {
    add    :: Maybe a,
    remove :: Maybe a
  } deriving (Eq, Foldable, Functor, Generic, Read, Show, Traversable)
