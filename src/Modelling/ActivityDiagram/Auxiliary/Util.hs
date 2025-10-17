{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Modelling.ActivityDiagram.Auxiliary.Util (
  finalNodesAdvice
  ) where

import Data.String.Interpolate          (iii)
import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  )

finalNodesAdvice :: OutputCapable m => LangM m
finalNodesAdvice = do
  paragraph $ translate $ do
    english [iii|
      Hint on the translation to a Petri net:
      For final nodes no additional places are introduced.
      They are realised in a way that a token is consumed,
      i.e. disappears from the net at that position.
      |]
    german [iii|
      Hinweis zur Übersetzung in ein Petrinetz:
      Für Endknoten  werden keine zusätzlichen Stellen eingeführt.
      Sie werden so realisiert, dass ein Token verbraucht wird,
      also an dieser Position aus dem Netz verschwindet.
      |]
  pure ()
