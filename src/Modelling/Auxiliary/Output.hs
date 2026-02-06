{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  addPretext,
  checkTaskText,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  uniform,
  ) where

import qualified Data.Map                         as M (empty, insert)

import Control.OutputCapable.Blocks     (
  GenericOutputCapable (paragraph),
  Language(..),
  LangM,
  LangM',
  OutputCapable,
  english,
  german,
  translate,
  translations,
  collapsed,
  )
import Control.OutputCapable.Blocks.Type (
  SpecialOutput,
  checkTranslations,
  )
import Data.List                        ((\\), singleton)
import Data.Map                         (Map)
import Data.String.Interpolate          (iii)

hoveringInformation :: OutputCapable m => Bool -> LangM m
hoveringInformation isCollapsed = collapsed isCollapsed (translations $ do
  english "Note on hovering"
  german "Anmerkung zum Hovern"
  ) $ translate $ do
  english [iii|
    When hovering over or clicking on nodes / edges or their
    labels, the respective diagram elements that belong together are highlighted.
    |]
  german [iii|
    Beim Bewegen über oder Klicken auf
    Knoten / Kanten bzw. ihre Beschriftungen
    werden die jeweils zusammengehörenden Diagrammelemente hervorgehoben.
    |]

directionsAdvice :: OutputCapable m => Bool -> LangM m
directionsAdvice isCollapsed = collapsed isCollapsed (translations $ do
  english "Note on navigation directions"
  german "Anmerkung zu Navigationsrichtungen"
  ) $ translate $ do
  english [iii|
    Aggregations and compositions are only navigable
    from the "part" toward the "whole",
    i.e., they are not navigable in the opposite direction!
    |]
  german [iii|
    Aggregationen und Kompositionen
    sind nur vom "Teil" zum "Ganzen" navigierbar,
    d.h., sie sind nicht in der entgegengesetzten Richtung navigierbar!
    |]

simplifiedInformation :: OutputCapable m => Bool -> LangM m
simplifiedInformation isCollapsed = collapsed isCollapsed (translations $ do
  english "Note on class representation"
  german "Anmerkung zur Klassendarstellung"
  ) $ translate $ do
  english [iii|
    Classes are represented simplified here.
    #{endLine}
    That means they consist of a single box containing only the class name
    but no sections for attributes or methods.
    #{endLine}
    Nevertheless, you should treat these simplified class representations
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
    english "Remarks on the submitted solution:"
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
