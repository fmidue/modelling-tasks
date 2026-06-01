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
  (English, "A class diagram is only valid if it can actually be instantiated, i.e. if there exists at least one object diagram conforming to it. Note in particular that combinations of compositions can make a diagram non-instantiable, even though each relationship looks fine on its own."),
  (German, "Ein Klassendiagramm ist nur dann gültig, wenn es tatsächlich instanziiert werden kann, d. h. wenn es mindestens ein dazu passendes Objektdiagramm gibt. Beachten Sie insbesondere, dass Kombinationen von Kompositionen ein Diagramm nicht-instanziierbar machen können, obwohl jede Beziehung für sich genommen in Ordnung erscheint.")
  ]

validClassDiagramAdvice :: ExtraText
validClassDiagramAdvice = Collapsible True validClassDiagramAdviceTitle validClassDiagramAdviceText
