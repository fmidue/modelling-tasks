-- | Common definitions for Activity Diagram example configurations.

module Modelling.ActivityDiagram.Common (
  finalNodesAdviceTitle,
  finalNodesAdviceText,
  finalNodesAdvice,
  finalNodesAndTransitionsAdvice,
  ) where

import qualified Data.Map as M

import Modelling.Auxiliary.Output (ExtraText(..))
import Control.OutputCapable.Blocks (Language(..))

-- | Title for the final nodes advice collapsible section.
finalNodesAdviceTitle :: M.Map Language String
finalNodesAdviceTitle = M.fromList [
  (English, "Hint on the translation to a Petri net"),
  (German, "Hinweis zur Übersetzung in ein Petrinetz")
  ]

-- | The main text explaining how final nodes are realized in Petri nets.
finalNodesAdviceText :: M.Map Language String
finalNodesAdviceText = M.fromList [
  (English, "For final nodes no additional places are introduced. They are realised in a way that a token is consumed, i.e. disappears from the net at that position."),
  (German, "Für Endknoten werden keine zusätzlichen Stellen eingeführt. Sie werden so realisiert, dass ein Token verbraucht wird, also an dieser Position aus dem Netz verschwindet.")
  ]

-- | Advice text for final nodes in Petri net translation.
-- This text explains how final nodes are realized in Petri nets.
finalNodesAdvice :: ExtraText
finalNodesAdvice = Collapsible True finalNodesAdviceTitle finalNodesAdviceText

-- | Combined advice text for final nodes and auxiliary transitions.
-- This text explains how final nodes are realized in Petri nets and clarifies
-- that transitions required for realizing final node behavior do not count as auxiliary nodes.
finalNodesAndTransitionsAdvice :: ExtraText
finalNodesAndTransitionsAdvice = Collapsible True finalNodesAdviceTitle
  (M.fromList [
    (English, englishText),
    (German, germanText)
  ])
  where
    englishText = (finalNodesAdviceText M.! English) ++ " If an additional transition is required to realise this behavior, this transition does not count as auxiliary node."
    germanText = (finalNodesAdviceText M.! German) ++ " Falls eine zusätzliche Transition erforderlich ist, um dieses Verhalten zu realisieren, zählt diese Transition nicht als Hilfsknoten."
