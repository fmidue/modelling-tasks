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
            { classNames = [ "D", "B", "C", "A", "E" ]
            , relationships = 
              [ Aggregation 
                  { aggregationName = "u"
                  , aggregationPart = LimitedLinking 
                      { linking = "C", limits = ( 0, Just 1 ) }
                  , aggregationWhole = LimitedLinking 
                      { linking = "A", limits = ( 1, Just 1 ) } 
                  }
              , Composition 
                  { compositionName = "x"
                  , compositionPart = LimitedLinking 
                      { linking = "D", limits = ( 2, Just 2 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "C", limits = ( 0, Just 1 ) } 
                  }
              , Composition 
                  { compositionName = "t"
                  , compositionPart = LimitedLinking 
                      { linking = "B", limits = ( 1, Just 1 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "A", limits = ( 0, Just 1 ) } 
                  }
              , Inheritance { subClass = "B", superClass = "D" }
              , Association 
                  { associationName = "w"
                  , associationFrom = LimitedLinking 
                      { linking = "E", limits = ( 1, Just 1 ) }
                  , associationTo = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 2 ) } 
                  }
              , Inheritance { subClass = "E", superClass = "B" }
              , Association 
                  { associationName = "z"
                  , associationFrom = LimitedLinking 
                      { linking = "D", limits = ( 1, Nothing ) }
                  , associationTo = LimitedLinking 
                      { linking = "E", limits = ( 0, Just 1 ) } 
                  } 
              ] 
            } 
        )
      , ( 2
        , ClassDiagram 
            { classNames = [ "C", "B", "E", "A", "D" ]
            , relationships = 
              [ Association 
                  { associationName = "w"
                  , associationFrom = LimitedLinking 
                      { linking = "E", limits = ( 1, Just 1 ) }
                  , associationTo = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 2 ) } 
                  }
              , Association 
                  { associationName = "z"
                  , associationFrom = LimitedLinking 
                      { linking = "D", limits = ( 1, Nothing ) }
                  , associationTo = LimitedLinking 
                      { linking = "E", limits = ( 0, Just 1 ) } 
                  }
              , Composition 
                  { compositionName = "x"
                  , compositionPart = LimitedLinking 
                      { linking = "D", limits = ( 2, Just 2 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "C", limits = ( 0, Just 1 ) } 
                  }
              , Inheritance { subClass = "E", superClass = "B" }
              , Aggregation 
                  { aggregationName = "u"
                  , aggregationPart = LimitedLinking 
                      { linking = "A", limits = ( 0, Just 1 ) }
                  , aggregationWhole = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 1 ) } 
                  }
              , Composition 
                  { compositionName = "t"
                  , compositionPart = LimitedLinking 
                      { linking = "B", limits = ( 1, Just 1 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "A", limits = ( 0, Just 1 ) } 
                  }
              , Inheritance { subClass = "D", superClass = "B" } 
              ] 
            } 
        ) 
      ]
  , instances = fromList
      [ ( 'a'
        , ( [ ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "d1"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "a"
                    , objectClass = "A" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "d", objectClass = "D" }
                , Object 
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "e", objectClass = "E" } 
                ]
              , links = 
                [ Link { linkName = "x", linkFrom = "d", linkTo = "c" }
                , Link { linkName = "w", linkFrom = "c", linkTo = "e" }
                , Link { linkName = "x", linkFrom = "d1", linkTo = "c" }
                , Link { linkName = "t", linkFrom = "b", linkTo = "a" }
                , Link { linkName = "u", linkFrom = "c", linkTo = "a" }
                , Link { linkName = "z", linkFrom = "d1", linkTo = "e" } 
                ] 
              } 
          ) 
        )
      , ( 'b'
        , ( [ 2 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "e"
                    , objectClass = "E" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "d1"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "d"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "a", objectClass = "A" }
                , Object 
                    { isAnonymous = True, objectName = "b", objectClass = "B" } 
                ]
              , links = 
                [ Link { linkName = "u", linkFrom = "a", linkTo = "c" }
                , Link { linkName = "x", linkFrom = "d", linkTo = "c" }
                , Link { linkName = "w", linkFrom = "e", linkTo = "c" }
                , Link { linkName = "x", linkFrom = "d1", linkTo = "c" }
                , Link { linkName = "z", linkFrom = "d", linkTo = "e" }
                , Link { linkName = "t", linkFrom = "e", linkTo = "a" } 
                ] 
              } 
          ) 
        )
      , ( 'c'
        , ( [ 1 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "b1"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "a"
                    , objectClass = "A" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "d"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "c", objectClass = "C" }
                , Object 
                    { isAnonymous = True, objectName = "e", objectClass = "E" } 
                ]
              , links = 
                [ Link { linkName = "u", linkFrom = "c", linkTo = "a" }
                , Link { linkName = "z", linkFrom = "b1", linkTo = "e" }
                , Link { linkName = "w", linkFrom = "e", linkTo = "c" }
                , Link { linkName = "x", linkFrom = "b1", linkTo = "c" }
                , Link { linkName = "t", linkFrom = "e", linkTo = "a" }
                , Link { linkName = "x", linkFrom = "b", linkTo = "c" } 
                ] 
              } 
          ) 
        )
      , ( 'd'
        , ( [ ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = True, objectName = "a", objectClass = "A" }
                , Object 
                    { isAnonymous = True, objectName = "d", objectClass = "D" }
                , Object 
                    { isAnonymous = False
                    , objectName = "d1"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "e"
                    , objectClass = "E" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
                    } 
                ]
              , links = 
                [ Link { linkName = "t", linkFrom = "e", linkTo = "a" }
                , Link { linkName = "x", linkFrom = "d", linkTo = "c" }
                , Link { linkName = "x", linkFrom = "d1", linkTo = "c" }
                , Link { linkName = "u", linkFrom = "c", linkTo = "a" }
                , Link { linkName = "z", linkFrom = "d1", linkTo = "e" }
                , Link { linkName = "w", linkFrom = "c", linkTo = "e" } 
                ] 
              } 
          ) 
        )
      , ( 'e'
        , ( [ 1 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "e"
                    , objectClass = "E" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
                    }
                , Object 
                    { isAnonymous = True
                    , objectName = "d1"
                    , objectClass = "D" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "a", objectClass = "A" }
                , Object 
                    { isAnonymous = False
                    , objectName = "d"
                    , objectClass = "D" 
                    } 
                ]
              , links = 
                [ Link { linkName = "u", linkFrom = "c", linkTo = "a" }
                , Link { linkName = "t", linkFrom = "b", linkTo = "a" }
                , Link { linkName = "w", linkFrom = "e", linkTo = "c" }
                , Link { linkName = "x", linkFrom = "d1", linkTo = "c" }
                , Link { linkName = "z", linkFrom = "b", linkTo = "e" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c" } 
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
