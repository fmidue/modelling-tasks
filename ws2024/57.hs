  MatchCdOdInstance
    { cdDrawSettings = CdDrawSettings
        { omittedDefaults = OmittedDefaultMultiplicities
            { aggregationWholeOmittedDefaultMultiplicity = Just ( 0, Nothing )
            , associationOmittedDefaultMultiplicity = Just ( 0, Nothing )
            , compositionWholeOmittedDefaultMultiplicity = Nothing
            }
        , printNames = True
        , printNavigations = True
        }
    , diagrams = fromList
        [ ( 1
          , ClassDiagram
              { classNames = [ "C", "E", "B", "D", "A" ]
              , relationships =
                [ Inheritance { subClass = "E", superClass = "D" }
                , Inheritance { subClass = "A", superClass = "D" }
                , Aggregation
                    { aggregationName = "z"
                    , aggregationPart = LimitedLinking
                        { linking = "A", limits = ( 0, Just 1 ) }
                    , aggregationWhole = LimitedLinking
                        { linking = "E", limits = ( 0, Just 2 ) }
                    }
                , Aggregation
                    { aggregationName = "y"
                    , aggregationPart = LimitedLinking
                        { linking = "B", limits = ( 0, Just 1 ) }
                    , aggregationWhole = LimitedLinking
                        { linking = "C", limits = ( 0, Just 1 ) }
                    }
                , Association
                    { associationName = "u"
                    , associationFrom = LimitedLinking
                        { linking = "A", limits = ( 2, Nothing ) }
                    , associationTo = LimitedLinking
                        { linking = "C", limits = ( 0, Just 2 ) }
                    }
                , Composition
                    { compositionName = "v"
                    , compositionPart = LimitedLinking
                        { linking = "B", limits = ( 1, Just 2 ) }
                    , compositionWhole = LimitedLinking
                        { linking = "A", limits = ( 0, Just 1 ) }
                    }
                , Association
                    { associationName = "w"
                    , associationFrom = LimitedLinking
                        { linking = "E", limits = ( 2, Nothing ) }
                    , associationTo = LimitedLinking
                        { linking = "B", limits = ( 1, Just 2 ) }
                    }
                ]
              }
          )
        , ( 2
          , ClassDiagram
              { classNames = [ "C", "A", "D", "E", "B" ]
              , relationships =
                [ Composition
                    { compositionName = "v"
                    , compositionPart = LimitedLinking
                        { linking = "B", limits = ( 0, Just 2 ) }
                    , compositionWhole = LimitedLinking
                        { linking = "A", limits = ( 0, Just 1 ) }
                    }
                , Aggregation
                    { aggregationName = "z"
                    , aggregationPart = LimitedLinking
                        { linking = "A", limits = ( 0, Just 1 ) }
                    , aggregationWhole = LimitedLinking
                        { linking = "E", limits = ( 0, Just 2 ) }
                    }
                , Association
                    { associationName = "u"
                    , associationFrom = LimitedLinking
                        { linking = "A", limits = ( 2, Nothing ) }
                    , associationTo = LimitedLinking
                        { linking = "C", limits = ( 0, Just 2 ) }
                    }
                , Inheritance { subClass = "A", superClass = "D" }
                , Association
                    { associationName = "w"
                    , associationFrom = LimitedLinking
                        { linking = "E", limits = ( 2, Nothing ) }
                    , associationTo = LimitedLinking
                        { linking = "B", limits = ( 1, Just 2 ) }
                    }
                , Aggregation
                    { aggregationName = "y"
                    , aggregationPart = LimitedLinking
                        { linking = "B", limits = ( 0, Just 1 ) }
                    , aggregationWhole = LimitedLinking
                        { linking = "C", limits = ( 0, Just 1 ) }
                    }
                , Inheritance { subClass = "E", superClass = "D" }
                ]
              }
          )
        ]
    , instances = fromList
        [ ( 'a'
          , ( [ 2 ]
            , ObjectDiagram
                { objects =
                  [ Object
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "c"
                      , objectClass = "C"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e1"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = True, objectName = "a", objectClass = "A" }
                  , Object
                      { isAnonymous = False
                      , objectName = "e"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = True
                      , objectName = "a1"
                      , objectClass = "A"
                      }
                  ]
                , links =
                  [ Link { linkName = "z", linkFrom = "a1", linkTo = "e1" }
                  , Link { linkName = "v", linkFrom = "b", linkTo = "a1" }
                  , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                  , Link { linkName = "u", linkFrom = "a", linkTo = "c" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "u", linkFrom = "a1", linkTo = "c" }
                  ]
                }
            )
          )
        , ( 'b'
          , ( [ ]
            , ObjectDiagram
                { objects =
                  [ Object
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = True, objectName = "a", objectClass = "A" }
                  , Object
                      { isAnonymous = False
                      , objectName = "b1"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e2"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = True
                      , objectName = "e1"
                      , objectClass = "E"
                      }
                  ]
                , links =
                  [ Link { linkName = "w", linkFrom = "e2", linkTo = "b" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e2" }
                  , Link { linkName = "w", linkFrom = "e2", linkTo = "b1" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e1" }
                  , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b1" }
                  ]
                }
            )
          )
        , ( 'c'
          , ( [ 1, 2 ]
            , ObjectDiagram
                { objects =
                  [ Object
                      { isAnonymous = True, objectName = "e", objectClass = "E" }
                  , Object
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = True, objectName = "a", objectClass = "A" }
                  , Object
                      { isAnonymous = False
                      , objectName = "b1"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e1"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e2"
                      , objectClass = "E"
                      }
                  ]
                , links =
                  [ Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e2", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e2", linkTo = "b1" }
                  , Link { linkName = "v", linkFrom = "b", linkTo = "a" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b1" }
                  ]
                }
            )
          )
        , ( 'd'
          , ( [ ]
            , ObjectDiagram
                { objects =
                  [ Object
                      { isAnonymous = True
                      , objectName = "b1"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e1"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = True, objectName = "e", objectClass = "E" }
                  , Object
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e2"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "a"
                      , objectClass = "A"
                      }
                  ]
                , links =
                  [ Link { linkName = "w", linkFrom = "e1", linkTo = "b1" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b1" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e2" }
                  , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e1" }
                  , Link { linkName = "w", linkFrom = "e2", linkTo = "b" }
                  ]
                }
            )
          )
        , ( 'e'
          , ( [ 2 ]
            , ObjectDiagram
                { objects =
                  [ Object
                      { isAnonymous = True
                      , objectName = "e1"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = True
                      , objectName = "a1"
                      , objectClass = "A"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "a"
                      , objectClass = "A"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "e2"
                      , objectClass = "E"
                      }
                  , Object
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B"
                      }
                  ]
                , links =
                  [ Link { linkName = "z", linkFrom = "a1", linkTo = "e1" }
                  , Link { linkName = "z", linkFrom = "a1", linkTo = "e" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "e2" }
                  , Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e2", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                  , Link { linkName = "v", linkFrom = "b", linkTo = "a" }
                  ]
                }
            )
          )
        ]
    , showSolution = True
    , taskText =
      [ Paragraph
        [ Translated (fromList
            [ ( English, "Consider the following two (valid) class diagrams:" )
            , ( German
              , "Betrachten Sie die folgenden zwei (validen) Klassendiagramme:"
              )
            ])
        ]
      , Special GivenCds
      , Paragraph
        [ Translated (fromList
            [ ( English
              , "Which of the following five object diagrams conform to which class diagram? \n An object diagram can conform to neither, either, or both class diagrams."
              )
            , ( German
              , "Welche der folgenden f\252nf Objektdiagramme passen zu welchem Klassendiagramm? \n Ein Objektdiagramm kann zu keinem, einem oder beiden Klassendiagrammen passen."
              )
            ])
        ]
      , Special GivenOds
      , Paragraph
        [ Translated (fromList
            [ ( English
              , "Please state your answer by giving a list of pairs, each comprising of a class diagram number and object diagram letters. \n Each pair indicates that the mentioned object diagrams conform to the respective class diagram. \n For example, "
              )
            , ( German
              , "Bitte geben Sie Ihre Antwort in Form einer Liste von Paaren an, die jeweils aus einer Klassendiagrammnummer und aus Objektdiagrammbuchstaben bestehen. \n Jedes Paar gibt an, dass die genannten Objektdiagramme zu dem jeweiligen Klassendiagramm passen. \n Zum Beispiel dr\252ckt "
              )
            ])
        , Code (fromList
            [ ( English, "[(1,ab),(2,)]" ), ( German, "[(1,ab),(2,)]" ) ])
        , Translated (fromList
          [ ( English
            , "expresses that among the offered choices exactly the object diagrams a and b are instances of class diagram 1 and that none of the offered object diagrams are instances of class diagram 2." 
            )
          , ( German
            , "aus, dass unter den angebotenen Auswahlm\246glichkeiten genau die Objektdiagramme a und b Instanzen des Klassendiagramms 1 sind und dass keines der angebotenen Objektdiagramme Instanz des Klassendiagramms 2 ist." 
            ) 
          ]) 
      ] 
    ] 
  }
