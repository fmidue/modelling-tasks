{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Modelling.PetriNet.ConflictPlaces where

import Modelling.Auxiliary.Output (
  LangM',
  LangM,
  OutputMonad (..),
  Rated,
  continueOrAbort,
  english,
  german,
  hoveringInformation,
  translate,
  translations,
  )
import Modelling.PetriNet.BasicNetFunctions (
  checkConfigForFind,
  checkConflictConfig,
  prohibitHidePlaceNames,
  )
import Modelling.PetriNet.ConcurrencyAndConflict (
  ConflictPlaces,
  FindInstance (..),
  conflictPlacesShow,
  drawFindWith,
  findConflictEvaluation,
  findConflictSyntax,
  findInitial,
  net,
  renderWith,
  )
import Modelling.PetriNet.Reach.Type (
  Place (Place),
  ShowPlace (ShowPlace),
  ShowTransition (ShowTransition),
  Transition,
  parsePlacePrec,
  parseTransitionPrec,
  )
import Modelling.PetriNet.Types (
  Conflict,
  FindConflictConfig (..),
  defaultFindConcurrencyConfig,
  defaultFindConflictConfig,
  lBasicConfig,
  lHidePlaceNames,
  )

import Control.Applicative              ((<|>))
import Control.Lens                     ((.~))
import Control.Monad                    (forM_, void)
import Control.Monad.IO.Class           (MonadIO)
import Data.Bifunctor                   (Bifunctor (bimap))
import Data.Containers.ListUtils        (nubOrd)
import Data.Function                    ((&))
import Data.List                        (partition)
import Data.Ratio                       ((%))
import Data.String.Interpolate          (i)
import Text.Parsec (
  char,
  endBy1,
  optionMaybe,
  optional,
  spaces,
  )
import Text.Parsec.String               (Parser)

findConflictPlacesTask
  :: (MonadIO m, OutputMonad m)
  => FilePath
  -> FindInstance Conflict
  -> LangM m
findConflictPlacesTask path task = do
  pn <- renderWith path "conflict" (net task) (drawFindWith task)
  paragraph $ translate $ do
    english "Considering this Petri net"
    german "Betrachten Sie folgendes Petrinetz"
  image pn
  paragraph $ translate $ do
    english "Which pair of transitions are in conflict because of which places under the initial marking?"
    german "Welches Paar von Transitionen steht wegen welcher konfliktauslösenden Stellen unter der Startmarkierung in Konflikt?"
  paragraph $ do
    translate $ do
      english "Please state your answer by giving a pair of conflicting transitions and a list of places being sources of the conflict."
      german "Geben Sie Ihre Antwort durch Eingabe eines Paars von in Konflikt stehenden Transitionen und einer Liste von Stellen, die den Konflikt auslösen, an. "
    translate $ do
      english [i|Stating |]
      german [i|Die Eingabe von |]
    code $ show conflictInitialShow
    translate $ do
      let ((t1, t2), [p1, p2]) = bimap
            (bimap show show)
            (fmap show)
            conflictInitialShow
      english [i| as answer would indicate that transitions #{t1} and #{t2} are in conflict under the initial marking
and that places #{p1} and #{p2} are the reason for the conflict. |]
      german [i| als Antwort würde bedeuten, dass Transitionen #{t1} und #{t2} unter der Startmarkierung in Konflikt stehen
und dass die Stellen #{p1} und #{p2} den Konflikt auslösen. |]
    translate $ do
      english "The order of transitions within the pair does not matter here."
      german "Die Reihenfolge der Transitionen innerhalb des Paars spielt hierbei keine Rolle."
  paragraph hoveringInformation

conflictInitial :: ConflictPlaces
conflictInitial = (findInitial, [Place 0, Place 1])

conflictInitialShow :: ((ShowTransition, ShowTransition), [ShowPlace])
conflictInitialShow = conflictPlacesShow conflictInitial

findConflictPlacesSyntax
  :: OutputMonad m
  => FindInstance Conflict
  -> ConflictPlaces
  -> LangM' m ()
findConflictPlacesSyntax task (conflict, ps) = do
  findConflictSyntax task conflict
  forM_ ps $ \x -> assert (isValidPlace x) $ translate $ do
    let x' = show $ ShowPlace x
    english $ x' ++ " is a valid place of the given Petri net?"
    german $ x' ++ " ist eine gültige Stelle des gegebenen Petrinetzes?"
  where
    isValidPlace (Place x) = x >= 1 && x <= numberOfPlaces task
    assert = continueOrAbort $ showSolution task

parseConflictPlacesPrec :: Int -> Parser ConflictPlaces
parseConflictPlacesPrec _  = do
  spaces
  mo <- optionMaybe (char '(')
  x <- Left <$> parseConflict mo
    <|> Right <$> parsePlaces
  spaces
  optional (char ',')
  y <- either (\y -> (y,) <$> parsePlaces) (\y -> (,y) <$> parseConflict Nothing) x
  spaces
  optional (char ')')
  spaces
  return y
  where
    parseConflict mo = do
      spaces
      maybe void (const optional) mo $ char '('
      t1 <- parseTransitionPrec 0
      spaces
      void $ char ','
      t2 <- parseTransitionPrec 0
      spaces
      void $ char ')'
      return (t1, t2)
    parsePlaces =
      spaces
      *> char '['
      *> parsePlacePrec 0 `endBy1` (spaces <* optional (char ','))
      <*  char ']'

defaultFindConflictPlacesConfig :: FindConflictConfig
defaultFindConflictPlacesConfig = defaultFindConflictConfig
  & lBasicConfig . lHidePlaceNames .~ False

checkFindConflictPlacesConfig :: FindConflictConfig -> Maybe String
checkFindConflictPlacesConfig FindConflictConfig {
  basicConfig,
  changeConfig,
  conflictConfig
  }
  = prohibitHidePlaceNames basicConfig
  <|> checkConfigForFind basicConfig changeConfig
  <|> checkConflictConfig basicConfig conflictConfig
