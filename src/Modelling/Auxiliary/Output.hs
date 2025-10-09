{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  ExtraText(..),
  addPretext,
  checkTaskText,
  directionsAdvice,
  extra,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  ) where

import qualified Data.Map                         as M (empty, insert)

import Control.Monad.State (put)
import Control.OutputCapable.Blocks     (
  GenericOutputCapable (paragraph),
  Language(..),
  LangM,
  LangM',
  OutputCapable,
  english,
  german,
  translate,
  collapsed,
  )
import Control.OutputCapable.Blocks.Type (
  SpecialOutput,
  checkTranslations,
  )
import Data.List                        ((\\), singleton)
import Data.Map                         (Map)
import Data.String.Interpolate          (iii)
import Data.Data (Data)

hoveringInformation :: OutputCapable m => LangM m
hoveringInformation = collapsed True (put $ uniform "Note") $ translate $ do
  english [iii|
    When hovering over or clicking on edges / nodes or their
    labels, the respective components that belong together are highlighted.
    |]
  german [iii|
    Beim Bewegen über oder Klicken auf
    Kanten / Knoten bzw. ihre Beschriftungen
    werden die jeweils zusammengehörenden Komponenten hervorgehoben.
    |]

directionsAdvice :: OutputCapable m => LangM m
directionsAdvice = collapsed True (put $ uniform "Note") $ translate $ do
  english [iii|
    As navigation directions are used,
    aggregations and compositions are only navigable
    from the "part" toward the "whole",
    i.e., they are not navigable in the opposite direction!
    |]
  german [iii|
    Da Navigationsrichtungen verwendet werden,
    sind Aggregationen und Kompositionen
    nur vom "Teil" zum "Ganzen" navigierbar,
    d.h., sie sind nicht in der entgegengesetzten Richtung navigierbar!
    |]

simplifiedInformation :: OutputCapable m => LangM m
simplifiedInformation = collapsed True (put $ uniform "Note") $ translate $ do
  english [iii|
    Classes are represented simplified here.
    #{endLine}
    That means they consist of a single box containing only the class name
    but no sections for attributes or methods.
    #{endLine}
    Nevertheless you should treat these simplified class representations
    as valid classes.
    |]
  german [iii|
    Klassen werden hier vereinfacht dargestellt.
    #{endLine}
    Das heißt, sie bestehen aus einer einfachen Box,
    die nur den Klassennamen enthält,
    aber keine Abschnitte für Attribute oder Methoden.
    #{endLine}
    Trotzdem sollten Sie diese vereinfachten Klassendarstellungen
    als gültige Klassen ansehen.
    |]
  where
    endLine :: String
    endLine = "\n"

addPretext :: OutputCapable m => LangM' m a -> LangM' m a
addPretext = (*>) $
  paragraph $ translate $ do
    english "Remarks on your solution:"
    german "Anmerkungen zur eingereichten Lösung:"

uniform :: a -> Map Language a
uniform x = foldr (`M.insert` x) M.empty [minBound ..]

checkTaskText
  :: (Bounded element, Enum element, Eq element, Show element)
  => [SpecialOutput element]
  -> Maybe String
checkTaskText taskText
  | x:_ <- allElements \\ usedElements
  = Just [iii|Your task text is incomplete as it is missing '#{show x}'.|]
  | x:_ <- usedElements \\ allElements
  = Just [iii|
      Your task text is using '#{show x}' at least twice,
      but it should appear exactly once.
      |]
  | x:_ <- concatMap (checkTranslations (const [])) taskText
  = Just $ [iii|Problem within your task text: |] ++ x
  | otherwise
  = Nothing
  where
    usedElements = concatMap (concatMap singleton) taskText
    allElements = [minBound ..]

-- | Configuration options for additional text
data ExtraText
  = NoExtraText              -- ^ Provide no additional text.
  | Static                   -- ^ Provide additional text that is always shown.
      (Map Language String)  -- ^ The text do be displayed.
  | Collapsible              -- ^ Provide additional text that can be collapsed.
      Bool                   -- ^ The default collapse status of the text.
      (Map Language String)  -- ^ The summary of the text to be shown.
      (Map Language String)  -- ^ The text to be shown when not collapsed.
  deriving (Data, Eq, Read, Show)

extra :: OutputCapable m => ExtraText -> LangM m
extra NoExtraText = pure ()
extra (Static textMap) = paragraph $ translate $ put textMap
extra (Collapsible defaultState titleText contentText) =
  collapsed
    defaultState
    (put titleText)
    (translate $ put contentText)
