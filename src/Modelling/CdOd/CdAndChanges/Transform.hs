{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Modelling.CdOd.CdAndChanges.Transform (
  transform,
  transformChanges,
  transformGetNextFix,
  transformImproveCd,
  transformNoChanges,
  ) where

import Modelling.CdOd.Types (
  Cd,
  CdMutation (..),
  ClassConfig (..),
  ClassDiagram (..),
  LimitedLinking (..),
  Relationship (..),
  RelationshipMutation (..),
  RelationshipProperties (..),
  maxRelationships,
  relationshipName,
  towardsValidProperties,
  )

import Data.Bool                        (bool)
import Data.FileEmbed                   (embedStringFile)
import Data.Functor                     ((<&>))
import Data.List                        (intercalate, unzip4)
import Data.List.Extra                  (nubOrd)
import Data.Maybe                       (fromMaybe)
import Data.String.Interpolate          (__i, i, iii)

transformWith
  :: ClassConfig
  -> [CdMutation]
  -> Either (ClassDiagram String String) RelationshipProperties
  -> (Int, [String], String)
  -> String
transformWith config mutations cdOrProperties (cs, predicates, part) =
  removeLine $(embedStringFile "alloy/cd/relationshipLimits.als")
  ++ removeLines 13 $(embedStringFile "alloy/cd/generate.als")
  ++ changePredicate mutations
  ++ either givenClassDiagram (classDiagram config) cdOrProperties
  ++ part
  ++ createRunCommand config predicates cs

{-|
Create Alloy code for the generation of a single class diagram with the
given properties.
-}
transformNoChanges
  :: ClassConfig
  -> RelationshipProperties
  -> Maybe Bool
  -> String
transformNoChanges config properties withNonTrivialInheritance =
  transformWith config [] (Right properties) (0, [], part)
  where
    part = [__i|
      fact{
      #{nonTrivialInheritanceConstraint "Inheritance" "NonInheritance" withNonTrivialInheritance}
      }
      |]

nonTrivialInheritanceConstraint :: String -> String -> Maybe Bool -> String
nonTrivialInheritanceConstraint inheritances nonInheritances withNonTrivialInheritance =
  (`foldMap` trivialInheritance) $ \x -> [i|  #{withInheritance}
  #{x} i : #{inheritances} | i.to in ((#{nonInheritances} + #{inheritances}).from + #{nonInheritances}.to)|]
  where
    trivialInheritance = withNonTrivialInheritance
      <&> bool "no" "all"
    withInheritance = maybe
      ""
      (bool "" someInheritance)
      withNonTrivialInheritance
    someInheritance = [i|some Inheritance <: #{inheritances}|]

transform
  :: ClassConfig
  -> [CdMutation]
  -> RelationshipProperties
  -> Maybe Bool
  -> String
transform config mutations props withNonTrivialInheritance =
  transformWith config mutations (Right props)
  $ matchCdOdChanges config withNonTrivialInheritance

transformChanges
  :: ClassConfig
  -> [CdMutation]
  -> RelationshipProperties
  -> Maybe ClassConfig
  -> [RelationshipProperties]
  -> String
transformChanges config mutations props maybeConfig propsList =
  transformWith config mutations (Right props)
  $ changes maybeConfig propsList

transformImproveCd
  :: ClassDiagram String String
  -- ^ the generated CD
  -> ClassConfig
  -- ^ the configuration used for generating the CD
  -> [CdMutation]
  -- ^ the mutations that are allowed to be used
  -> RelationshipProperties
  -- ^ the properties of the original CD
  -> String
transformImproveCd cd config mutations properties
  = transformWith config mutations (Left cd)
  $ changes Nothing [towardsValidProperties properties]

{-|
Generates Alloy code that

 * provides a change that removes an illegal relationship
 * makes sure, that no non-inheritance relationship exists twice within the
   class diagram if they are not referenced by name
-}
transformGetNextFix
  :: Maybe Cd
  -> ClassConfig
  -> RelationshipProperties
  -> Bool
  -> String
transformGetNextFix maybeCd config properties byName = transformWith
  config
  [RemoveRelationship]
  (maybe (Right properties) Left maybeCd)
  (n, ps, part ++ restrictRelationships)
  where
    (n, ps, part) = changes
      Nothing
      [towardsValidProperties properties]
    restrictRelationships =
      if byName
      then ""
      else [i|
fact preventSameNonInheritances {
  no disj x, y : NonInheritance |
    sameRelationship[x, y]
}
|]

nameRelationships
  :: ClassDiagram className String
  -> [(String, Relationship className String)]
nameRelationships ClassDiagram {relationships} = zipWith
  addName
  (map (("Relationship" ++) . show) [0 :: Int ..])
  relationships
  where
    addName defaultName r = (fromMaybe defaultName $ relationshipName r, r)

givenClassDiagram :: ClassDiagram String String -> String
givenClassDiagram cd@ClassDiagram {classNames} = [i|
//////////////////////////////////////////////////
// Given CD
//////////////////////////////////////////////////

#{concatMap classSig classNames}
#{concatMap relationshipSig namedRelationships}
pred cd {
  Class = #{unionOf classNames}
#{concatMap relationshipConstraints namedRelationships}
  NonInheritance = Association + Aggregation + Composition
  Relationship = NonInheritance + Inheritance
  Association - Change.add = #{unionOf $ concat associations}
  Aggregation - Change.add = #{unionOf $ concat aggregations}
  Composition - Change.add = #{unionOf $ concat compositions}
  Inheritance - Change.add = #{unionOf $ concat inheritances}
}
|]
  where
    unionOf xs
      | null xs = "none"
      | otherwise = intercalate " + " xs
    namedRelationships = nameRelationships cd
    (associations, aggregations, compositions, inheritances) =
      unzip4 $ map nonInheritanceName namedRelationships
    nonInheritanceName (name, x) = case x of
      Association {} -> ([name], [], [], [])
      Aggregation {} -> ([], [name], [], [])
      Composition {} -> ([], [], [name], [])
      Inheritance {} -> ([], [], [], [name])
    classSig :: String -> String
    classSig x = [i|one sig #{x} extends Class {}\n|]
    relationshipSig :: (String, Relationship String relationship) -> String
    relationshipSig (name, x) = case x of
      Association {} -> [i|one sig #{name} extends Association {}\n|]
      Aggregation {} -> [i|one sig #{name} extends Aggregation {}\n|]
      Composition {} -> [i|one sig #{name} extends Composition {}\n|]
      Inheritance {} -> [i|one sig #{name} extends Inheritance {}\n|]
    relationshipConstraints (name, x) = case x of
      Association {..} -> limitsConstraints name associationFrom associationTo
      Aggregation {..} -> limitsConstraints name aggregationPart aggregationWhole
      Composition {..} -> limitsConstraints name compositionPart compositionWhole
      Inheritance {..} -> [i|  #{name}.from = #{subClass}\n|]
        ++ [i|  #{name}.to = #{superClass}\n|]
    limitsConstraints x from to =
      limitConstraints "from" x from ++ limitConstraints "to" x to
    limitConstraints :: String -> String -> LimitedLinking String -> String
    limitConstraints
      what
      x
      LimitedLinking {linking = destination, limits = (low, high)} =
        [i|  #{x}.#{what} = #{destination}\n|]
        ++ [i|  #{x}.#{what}Lower = #{limit low}\n|]
        ++ [i|  #{x}.#{what}Upper = #{limit $ fromMaybe (-1) high}\n|]
    limit 0 = "Zero"
    limit 1 = "One"
    limit 2 = "Two"
    limit _ = "Star"

classDiagram :: ClassConfig -> RelationshipProperties -> String
classDiagram config props = [i|
//////////////////////////////////////////////////
// Basic CD
//////////////////////////////////////////////////

pred cd {
  let NonInheritance2 = NonInheritance - Change.add,
      Association2 = Association - Change.add,
      Aggregation2 = Aggregation - Change.add,
      Composition2 = Composition - Change.add,
      Relationship2 = Relationship - Change.add,
      Inheritance2 = Inheritance - Change.add {
    classDiagram [NonInheritance2, Composition2, Inheritance2, Relationship2,
      #{wrongNonInheritances props}, #{wrongCompositions props}, #{selfRelationships props},
      #{selfInheritances props},
      #{maybeToAlloySet $ hasDoubleRelationships props},
      #{maybeToAlloySet $ hasReverseRelationships props},
      #{hasReverseInheritances props},
      #{maybeToAlloySet $ hasMultipleInheritances props},
      #{hasNonTrivialInheritanceCycles props},
      #{hasCompositionCycles props},
      #{maybeToAlloySet $ hasCompositionsPreventingParts props},
      #{maybeToAlloySet $ hasThickEdges props}]
    #{fst $ associationLimits config} <= \#Association2
    \#Association2 <= #{upper $ associationLimits config}
    #{fst $ aggregationLimits config} <= \#Aggregation2
    \#Aggregation2 <= #{upper $ aggregationLimits config}
    #{fst $ compositionLimits config} <= \#Composition2
    \#Composition2 <= #{upper $ compositionLimits config}
    #{fst $ inheritanceLimits config} <= \#Inheritance2
    \#Inheritance2 <= #{upper $ inheritanceLimits config}
    #{fst $ relationshipLimits config} <= \#Relationship2
    \#Relationship2 <= #{upper $ relationshipLimits config}
    #{fst $ classLimits config} <= \#Class
  }
}
|]
  where
    upper = fromMaybe (maxRelationships config) . snd

maybeToAlloySet :: Show a => Maybe a -> String
maybeToAlloySet = maybe "none" show

changePredicate :: [CdMutation] -> String
changePredicate [] = [__i|
  pred change [c : Change, rs : set Relationship] {
    one c.add and no c.add
  }
  |]
changePredicate allowed = [__i|
  pred change [c : Change, rs : set Relationship] {
    some c.add + c.remove
    #{mutationConstraints}
    no c.add or not c.add in rs
    c.remove in rs
  }
  |]
  where
    mutationConstraints = intercalate " or "
      $ map changeConstraint $ nubOrd allowed
    changeConstraint :: CdMutation -> String
    changeConstraint change = case change of
      AddRelationship -> [iii|one c.add and no c.remove|]
      MutateRelationship mutation -> [iii|
        one c.add and one c.remove and #{mutationConstraint mutation}
        |]
      RemoveRelationship -> [iii|no c.add and one c.remove|]
    mutationConstraint :: RelationshipMutation -> String
    mutationConstraint mutation = case mutation of
      ChangeLimit -> [iii|changedLimit [c]|]
      ChangeKind -> [iii|changedKind [c]|]
      Flip -> [iii|flip [c]|]

changes
  :: Maybe ClassConfig
  -> [RelationshipProperties]
  -> (Int, [String], String)
changes config propsList = uncurry (length propsList,,)
  $ snd $ foldl change (1, limits) propsList
  where
    change (n, (cs, code)) p =
      let (c, code') = changeWithProperties p n
      in (n + 1, (c:cs, code ++ code'))
    limits = maybe
      ([], header)
      ((["changeLimits"],) . changeLimits)
      config
    header = [i|
//////////////////////////////////////////////////
// Changes
//////////////////////////////////////////////////
|]

changeWithProperties :: RelationshipProperties -> Int -> (String, String)
changeWithProperties props n = (change, alloy)
  where
    change = [i|change#{n}|]
    alloy =  [i|
sig C#{n} extends Change {}

pred #{change} {
  changeOfFirstCD [C#{n},
    #{wrongNonInheritances props}, #{wrongCompositions props}, #{selfRelationships props},
    #{selfInheritances props},
    #{maybeToAlloySet $ hasDoubleRelationships props},
    #{maybeToAlloySet $ hasReverseRelationships props},
    #{hasReverseInheritances props},
    #{maybeToAlloySet $ hasMultipleInheritances props},
    #{hasNonTrivialInheritanceCycles props},
    #{hasCompositionCycles props},
    #{maybeToAlloySet $ hasCompositionsPreventingParts props},
    #{maybeToAlloySet $ hasThickEdges props}]
}
|]

matchCdOdChanges
  :: ClassConfig
  -> Maybe Bool
  -> (Int, [String], String)
matchCdOdChanges config withNonTrivialInheritance =
  (3, ["changes", "changeLimits"],) $ [i|
//////////////////////////////////////////////////
// Changes
//////////////////////////////////////////////////
sig C1, C2, C3 extends Change {}

pred changes {
  one m1, m2 : Boolean {
    m1 = False or m2 = False
    let c1NonInheritances = NonInheritance - (Change.add - NonInheritance <: C1.add) - C1.remove,
        c2NonInheritances = NonInheritance - (Change.add - NonInheritance <: C2.add) - C2.remove {
      some c1NonInheritances or some c2NonInheritances
      let c1Inheritances = Inheritance - (Change.add - Inheritance <: C1.add) - C1.remove,
          c2Inheritances = Inheritance - (Change.add - Inheritance <: C2.add) - C2.remove {
        #{nonTrivialInheritanceConstraint "c1Inheritances" "c1NonInheritances" withNonTrivialInheritance}
        #{nonTrivialInheritanceConstraint "c2Inheritances" "c2NonInheritances" withNonTrivialInheritance}
      }
    }
    changeOfFirstCD [C1, 0, 0, 0, 0, False, False, False, False, False, False, False, m1]
    changeOfFirstCD [C2, 0, 0, 0, 0, False, False, False, False, False, False, False, m2]
    changeOfFirstCD [C3, 0, 0, 0, 0, False, False, False, False, False, False, False, False]
  }
}
|] ++ changeLimits config

changeLimits :: ClassConfig -> String
changeLimits config = [i|
pred changeLimits {
  all c : Change {
    let Association2 = Association - (Change.add - c.add) - c.remove,
        Composition2 = Composition - (Change.add - c.add) - c.remove,
        Aggregation2 = Aggregation - (Change.add - c.add) - c.remove,
        Inheritance2 = Inheritance - (Change.add - c.add) - c.remove {
      #{fst $ associationLimits config} <= \#Association2
      \#Association2 <= #{upper $ associationLimits config}
      #{fst $ aggregationLimits config} <= \#Aggregation2
      \#Aggregation2 <= #{upper $ aggregationLimits config}
      #{fst $ compositionLimits config} <= \#Composition2
      \#Composition2 <= #{upper $ compositionLimits config}
      #{fst $ inheritanceLimits config} <= \#Inheritance2
      \#Inheritance2 <= #{upper $ inheritanceLimits config}
    }
  }
}
|]
  where
    upper = fromMaybe (maxRelationships config) . snd

createRunCommand :: ClassConfig -> [String] -> Int ->  String
createRunCommand config@ClassConfig {..} predicates cs = [i|
run { #{command} } for #{relationships} Relationship, #{bitSize} Int,
  #{exactClass}#{snd classLimits} Class, exactly #{cs} Change
|]
  where
    exactClass
      | uncurry (==) classLimits = "exactly "
      | otherwise            = ""
    relMax = fromMaybe (maxRelationships config) . snd $ relationshipLimits
    relationships = relMax + cs
    bitSize :: Int
    bitSize = (+ 1) . ceiling @Double . logBase 2 . fromIntegral
      $ max relationships (snd classLimits) + 1
    command :: String
    command = foldl ((++) . (++ " and ")) "cd" predicates

removeLines :: Int -> String -> String
removeLines n
  | n < 0     = id
  | otherwise = removeLines (n - 1) . removeLine

removeLine :: String -> String
removeLine = drop 1 . dropWhile (/= '\n')
