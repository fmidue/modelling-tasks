module Modelling.CdOd.Common (
  validClassDiagramAdviceTitle,
  validClassDiagramAdviceText,
  validClassDiagramAdvice,
  ) where

import qualified Data.Map as M
import Control.OutputCapable.Blocks (ExtraText (..), Language (..))

validClassDiagramAdviceTitle :: M.Map Language String
validClassDiagramAdviceTitle = M.fromList [
  (English, "Hint on the validity of class diagrams"),
  (German, "Hinweis zur Gültigkeit von Klassendiagrammen")
  ]

validClassDiagramAdviceText :: M.Map Language String
validClassDiagramAdviceText = M.fromList [
  (English, "One reason for not considering a class diagram valid is if it cannot actually be instantiated, i.e., if there exists no object diagram conforming to it."),
  (German, "Ein Grund dafür, ein Klassendiagramm nicht als gültig anzusehen ist, wenn es gar nicht instanziiert werden kann, d.h., wenn kein dazu passendes Objektdiagramm existiert.")
  ]

validClassDiagramAdvice :: ExtraText
validClassDiagramAdvice = Collapsible True validClassDiagramAdviceTitle validClassDiagramAdviceText
