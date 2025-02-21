DifferentNamesInstance 
  { cDiagram = ClassDiagram 
      { classNames = [ "E", "A", "D", "C", "B" ]
      , relationships = 
        [ Composition 
            { compositionName = "d"
            , compositionPart = LimitedLinking 
                { linking = "B", limits = ( 1, Nothing ) }
            , compositionWhole = LimitedLinking 
                { linking = "D", limits = ( 0, Just 1 ) } 
            }
        , Inheritance { subClass = "C", superClass = "E" }
        , Aggregation 
            { aggregationName = "e"
            , aggregationPart = LimitedLinking 
                { linking = "B", limits = ( 1, Nothing ) }
            , aggregationWhole = LimitedLinking 
                { linking = "A", limits = ( 0, Just 2 ) } 
            }
        , Inheritance { subClass = "D", superClass = "E" }
        , Association 
            { associationName = "b"
            , associationFrom = LimitedLinking 
                { linking = "B", limits = ( 1, Just 2 ) }
            , associationTo = LimitedLinking 
                { linking = "C", limits = ( 2, Nothing ) } 
            }
        , Aggregation 
            { aggregationName = "a"
            , aggregationPart = LimitedLinking 
                { linking = "A", limits = ( 0, Nothing ) }
            , aggregationWhole = LimitedLinking 
                { linking = "D", limits = ( 2, Nothing ) } 
            }
        , Association 
            { associationName = "c"
            , associationFrom = LimitedLinking 
                { linking = "D", limits = ( 1, Nothing ) }
            , associationTo = LimitedLinking 
                { linking = "C", limits = ( 2, Just 2 ) } 
            }
        , Inheritance { subClass = "B", superClass = "E" }
        , Composition 
            { compositionName = "f"
            , compositionPart = LimitedLinking 
                { linking = "C", limits = ( 1, Nothing ) }
            , compositionWhole = LimitedLinking 
                { linking = "A", limits = ( 0, Just 1 ) } 
            } 
        ] 
      }
  , cdDrawSettings = CdDrawSettings 
      { omittedDefaults = OmittedDefaultMultiplicities 
          { aggregationWholeOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , associationOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , compositionWholeOmittedDefaultMultiplicity = Nothing 
          }
      , printNames = True
      , printNavigations = True 
      }
  , oDiagram = ObjectDiagram 
      { objects = 
        [ Object { isAnonymous = True, objectName = "b", objectClass = "B" }
        , Object { isAnonymous = True, objectName = "d", objectClass = "D" }
        , Object { isAnonymous = True, objectName = "c2", objectClass = "C" }
        , Object { isAnonymous = True, objectName = "c", objectClass = "C" }
        , Object { isAnonymous = True, objectName = "c1", objectClass = "C" }
        , Object { isAnonymous = True, objectName = "b1", objectClass = "B" }
        , Object { isAnonymous = True, objectName = "a", objectClass = "A" }
        , Object { isAnonymous = True, objectName = "d1", objectClass = "D" } 
        ]
      , links = 
        [ Link { linkName = "x", linkFrom = "d", linkTo = "c" }
        , Link { linkName = "y", linkFrom = "b1", linkTo = "a" }
        , Link { linkName = "w", linkFrom = "a", linkTo = "d" }
        , Link { linkName = "w", linkFrom = "a", linkTo = "d1" }
        , Link { linkName = "x", linkFrom = "d", linkTo = "c2" }
        , Link { linkName = "z", linkFrom = "b1", linkTo = "c1" }
        , Link { linkName = "v", linkFrom = "c", linkTo = "a" }
        , Link { linkName = "x", linkFrom = "d1", linkTo = "c1" }
        , Link { linkName = "z", linkFrom = "b1", linkTo = "c2" }
        , Link { linkName = "u", linkFrom = "b1", linkTo = "d1" }
        , Link { linkName = "x", linkFrom = "d1", linkTo = "c" }
        , Link { linkName = "u", linkFrom = "b", linkTo = "d" }
        , Link { linkName = "z", linkFrom = "b", linkTo = "c2" }
        , Link { linkName = "z", linkFrom = "b1", linkTo = "c" }
        , Link { linkName = "z", linkFrom = "b", linkTo = "c1" } 
        ] 
      }
  , showSolution = True
  , mapping =
      [ ( Name {unName = "a"}, Name {unName = "w"} ), ( Name {unName = "b"}, Name {unName = "z"} ), ( Name {unName = "c"}, Name {unName = "x"} ), ( Name {unName = "d"}, Name {unName = "u"} ), ( Name {unName = "e"}, Name {unName = "y"} ), ( Name {unName = "f"}, Name {unName = "v"} ) ]
  , linkShuffling = ConsecutiveLetters
  , taskText = 
    [ Paragraph 
      [ Translated (fromList
          [ ( English, "Consider the following (valid) class diagram:" )
          , ( German, "Betrachten Sie folgendes (valide) Klassendiagramm:" ) 
          ]) 
      ]
    , Special GivenCd
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "and the following object diagram (which conforms to it):" 
            )
          , ( German, "und das folgende (dazu passende) Objektdiagramm:" ) 
          ]) 
      ]
    , Special GivenOd
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Which relationship in the class diagram (CD) corresponds to which of the links in the object diagram (OD)? \n State your answer by giving a mapping of relationships in the CD to links in the OD. \n To state that a in the CD corresponds to x in the OD and b in the CD corresponds to y in the OD, write the mapping as:" 
            )
          , ( German
            , "Welche Beziehung im Klassendiagramm (CD) entspricht welchen Links im Objektdiagramm (OD)? \n Geben Sie Ihre Antwort als eine Zuordnung von Beziehungen im CD zu Links im OD an. \n Um anzugeben, dass a im CD zu x im OD und b im CD zu y im OD korrespondieren, schreiben Sie die Zuordnung als:" 
            ) 
          ])
      , Code (fromList
          [ ( English, "[(a,x),(b,y)]" ), ( German, "[(a,x),(b,y)]" ) ]) 
      ]
    , Special MappingAdvice 
    ]
  , usesAllRelationships = True 
  }
