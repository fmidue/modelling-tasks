module Modelling.CdOd.Common (
  classDiagramFirstHintTitle,
  classDiagramFirstHintText,
  classDiagramFirstHint,
  validClassDiagramAdviceTitle,
  validClassDiagramAdviceText,
  validClassDiagramAdvice
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
  (German, "Ein Grund dafür, ein Klassendiagramm nicht als gültig anzusehen, ist, wenn es gar nicht instanziiert werden kann, d.h., wenn kein dazu passendes Objektdiagramm existiert.")
  ]

validClassDiagramAdvice :: ExtraText
validClassDiagramAdvice = Collapsible True validClassDiagramAdviceTitle validClassDiagramAdviceText

classDiagramFirstHintTitle :: M.Map Language String
classDiagramFirstHintTitle = M.fromList [
  (English, "Hint on correctly solving the task"),
  (German, "Hinweis zur korrekten Lösung der Aufgabe")
  ]

classDiagramFirstHintText :: M.Map Language String
classDiagramFirstHintText = M.fromList [
  (English, "When solving the task, consider first creating a class diagram based on the given scenario description, before answering."),
  (German, "Wenn Sie die Aufgabe lösen, sollten Sie zuerst ein Klassendiagramm basierend auf der gegebenen Szenariobeschreibung erstellen, bevor Sie die Aufgabe beantworten.")
  ]

classDiagramFirstHint :: ExtraText
classDiagramFirstHint = Collapsible True classDiagramFirstHintTitle classDiagramFirstHintText
