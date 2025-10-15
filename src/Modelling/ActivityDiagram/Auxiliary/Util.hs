{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice
  ) where

import Data.String.Interpolate          (iii)
import Control.Monad.State              (put)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  collapsed,
  english,
  german,
  translate,
  translations,
  )

finalNodesAdvice :: OutputCapable m => LangM m
finalNodesAdvice = collapsed True (put $ translations $ do
  english "Hint on translation to Petri net"
  german "Hinweis zur Übersetzung in ein Petrinetz"
  ) $ translate $ do
  english [iii|
    For final nodes no additional places are introduced.
    They are realised in a way that a token is consumed,
    i.e. disappears from the net at that position.
    |]
  german [iii|
    Für Endknoten  werden keine zusätzlichen Stellen eingeführt.
    Sie werden so realisiert, dass ein Token verbraucht wird,
    also an dieser Position aus dem Netz verschwindet.
    |]
