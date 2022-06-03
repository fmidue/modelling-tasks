{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
-- | This module provides common skeletons for printing tasks
module Modelling.Auxiliary.Output (
  addPretext,
  directionsAdvice,
  hoveringInformation,
  simplifiedInformation,
  ) where


import Control.Monad.Output             (
  OutputMonad (paragraph),
  LangM,
  LangM',
  english,
  german,
  translate,
  )
import Data.String.Interpolate          (i)

hoveringInformation :: OutputMonad m => LangM m
hoveringInformation = translate $ do
  english [i|Please note: Hovering over or clicking on edges / nodes or their labels highlights the respective matching parts.|]
  german [i|Bitte beachten Sie: Beim Bewegen über oder Klicken auf Kanten / Knoten bzw. ihre Beschriftungen werden die jeweils zusammengehörenden Komponenten hervorgehoben.|]

directionsAdvice :: OutputMonad m => LangM m
directionsAdvice = translate $ do
  english [i|As navigation directions are used, please note that aggregations and compositions are only navigable from the "part" toward the "whole", i.e. they are not navigable in the opposite direction!|]
  german [i|Da Navigationsrichtungen verwendet werden, beachten Sie bitte, dass Aggregationen und Kompositionen nur vom "Teil" zum "Ganzen" navigierbar sind, d.h. sie sind nicht in der entgegengesetzten Richtung navigierbar!|]

simplifiedInformation :: OutputMonad m => LangM m
simplifiedInformation = translate $ do
  english [i|Please note: Classes are represented simplified here.
That means they consist of a single box containing only its class name, but do not contain boxes for attributes and methods.
Nevertheless you should treat these simplified class representations as valid classes.|]
  german [i|Bitte beachten Sie: Klassen werden hier vereinfacht dargestellt.
Das heißt, sie bestehen aus einer einfachen Box, die nur den Klassennamen enthält, aber keine Abschnitte für Attribute oder Methoden.
Trotzdem sollten Sie diese vereinfachten Klassendarstellungen als valide Klassen ansehen.|]

addPretext :: OutputMonad m => LangM' m a -> LangM' m a
addPretext = (>>) $
  paragraph $ translate $ do
    english "Remarks on your solution:"
    german "Anmerkungen zur eingereichten Lösung:"

