{-# LANGUAGE TemplateHaskell #-}

module Transform (transform) where

import Util
import Types

import Data.List
import Data.FileEmbed

transform :: String -> Bool -> String -> ([(String, Maybe String)], [Association]) -> String
transform time template index (classes, associations) = unlines $
  concat
  [
    [ "// Alloy Model for CD" ++ index
    , "// Produced by Haskell simulation of Eclipse plugin"
    , "// Generated: " ++ time
    , ""
    , "module umlp2alloy/CD" ++ index ++ "Module"
    , ""
    , $(embedStringFile "Template.txt")
    ]
  | template ]
  ++
  [ "// Concrete names of fields"
  , unlines (associationSigs associations) -- Figure 2.1, Rule 3, part 2
  , "// Classes"
  , unlines (classSigs classNames) -- Figure 2.1, Rule 1, part 1
  , "///////////////////////////////////////////////////"
  , "// CD" ++ index
  , "///////////////////////////////////////////////////"
  , ""
  , "// Types wrapping subtypes"
  , unlines (subTypes index classesWithDirectSubclasses) -- Figure 2.1, Rule 1, part 2, alternative implementation
  , "// Types wrapping field names"
  , unlines (fieldNames index associations classes) -- Figure 2.2, Rule 2, relevant portion, alternative implementation
  , "// Types wrapping composite structures and field names"
  , unlines (compositesAndFieldNames index compositions classes) -- Figure 2.1, Rule 6, corrected
  , "// Properties"
  , unlines (predicate index associations classNames)
  ]
  ++
  if template then
    [ "///////////////////////////////////////////////////"
    , "// Run commands"
    , "///////////////////////////////////////////////////"
    , ""
    , "run cd" ++ index ++ " for 5"
    ]
  else []
  where
    classNames = map fst classes
    classesWithDirectSubclasses = map (\(name, _) -> (name, map fst (filter ((== Just name) . snd) classes))) classes
    compositions = filter (\(a,_,_,_,_,_) -> a == Composition) associations

associationSigs :: [Association] -> [String]
associationSigs = map (\(_,name,_,_,_,_) -> "one sig " ++ firstLower name ++ " extends FName {}")

classSigs :: [String] -> [String]
classSigs = map (\name -> "sig " ++ name ++ " extends Obj {}")

subTypes :: String -> [(String, [String])] -> [String]
subTypes index = concatMap (\(name, directSubclasses) ->
    [ "fun " ++ name ++ subsCD ++ ": set Obj {"
    , "  " ++ intercalate " + " (name : map (++ subsCD) directSubclasses)
    , "}"
    ])
  where
    subsCD = "SubsCD" ++ index

fieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
fieldNames index associations = concatMap (\(this, super) ->
  let thisAssociations = filter (\(_,_,_,from,_,_) -> from == this) associations
  in
    [ "fun " ++ this ++ fieldNamesCD ++": set FName {"
    , "  " ++ intercalate " + " (maybe "none" (++ fieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> firstLower name) thisAssociations)
    , "}"
    ])
  where
    fieldNamesCD = "FieldNamesCD" ++ index

compositesAndFieldNames :: String -> [Association] -> [(String, Maybe String)] -> [String]
compositesAndFieldNames index compositions = concatMap (\(this, super) ->
  let thisCompositions = filter (\(_,_,_,_,to,_) -> to == this) compositions
  in
    [ "fun " ++ this ++ compositesCD ++ ": set Obj {"
    , "  " ++ intercalate " + " (maybe "none" (++ compositesCD) super
                                  : map (\(_,_,_,from,_,_) -> from ++ subsCD) thisCompositions)
    , "}"
    , "fun " ++ this ++ compFieldNamesCD ++ ": set FName {"
    , "  " ++ intercalate " + " (maybe "none" (++ compFieldNamesCD) super
                                  : map (\(_,name,_,_,_,_) -> firstLower name) thisCompositions)
    , "}"
    ])
  where
    compositesCD = "CompositesCD" ++ index
    compFieldNamesCD = "CompFieldNamesCD" ++ index
    subsCD = "SubsCD" ++ index

predicate :: String -> [Association] -> [String] -> [String]
predicate index associations classNames =
  [ "pred cd" ++ index ++ " {"
  , ""
  , "  Obj = " ++ intercalate " + " classNames -- Figure 2.2, Rule 5
  , ""
  , "  // Contents"
  , unlines objFNames
  , "  // Associations"
  , unlines objAttribs -- Figure 2.3, Rule A3
  , "  // Compositions"
  , unlines compositions -- Figure 2.2, Rule 4, corrected
  , "}"
  ]
  where objFNames = map (\name -> "  ObjFNames[" ++ name ++ ", " ++ name ++ fieldNamesCD ++ "]") classNames
        objAttribs = concatMap (\(_, name, mult1, class1, class2, mult2) -> [makeAssoc "Attrib" class1 name class2 mult2, makeAssoc "" class2 name class1 mult1]) associations
        makeAssoc att from name to (low, Nothing) = "  ObjL" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ "]"
        makeAssoc att from name to (low, Just up) = "  ObjLU" ++ att ++ "[" ++ from ++ subsCD ++ ", " ++ firstLower name ++ ", " ++ to ++ subsCD ++ ", " ++ show low ++ ", " ++ show up ++ "]"
        compositions = map (\name -> "  Composition[" ++ name ++ compositesCD ++ ", " ++ name ++ compFieldNamesCD ++ ", " ++ name ++ "]") classNames
        fieldNamesCD = "FieldNamesCD" ++ index
        compositesCD = "CompositesCD" ++ index
        compFieldNamesCD = "CompFieldNamesCD" ++ index
        subsCD = "SubsCD" ++ index
