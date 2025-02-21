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
            { classNames = [ "B", "A", "E", "C", "D" ]
            , relationships = 
              [ Composition 
                  { compositionName = "y"
                  , compositionPart = LimitedLinking 
                      { linking = "B", limits = ( 1, Just 1 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "A", limits = ( 1, Just 1 ) } 
                  }
              , Inheritance { subClass = "B", superClass = "D" }
              , Aggregation 
                  { aggregationName = "w"
                  , aggregationPart = LimitedLinking 
                      { linking = "E", limits = ( 0, Just 2 ) }
                  , aggregationWhole = LimitedLinking 
                      { linking = "B", limits = ( 0, Nothing ) } 
                  }
              , Composition 
                  { compositionName = "x"
                  , compositionPart = LimitedLinking 
                      { linking = "E", limits = ( 1, Just 2 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "C", limits = ( 0, Just 1 ) } 
                  }
              , Association 
                  { associationName = "z"
                  , associationFrom = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 2 ) }
                  , associationTo = LimitedLinking 
                      { linking = "A", limits = ( 1, Nothing ) } 
                  }
              , Inheritance { subClass = "E", superClass = "D" }
              , Association 
                  { associationName = "v"
                  , associationFrom = LimitedLinking 
                      { linking = "C", limits = ( 0, Nothing ) }
                  , associationTo = LimitedLinking 
                      { linking = "B", limits = ( 0, Just 1 ) } 
                  } 
              ] 
            } 
        )
      , ( 2
        , ClassDiagram 
            { classNames = [ "E", "D", "A", "B", "C" ]
            , relationships = 
              [ Composition 
                  { compositionName = "w"
                  , compositionPart = LimitedLinking 
                      { linking = "E", limits = ( 0, Just 2 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "B", limits = ( 0, Just 1 ) } 
                  }
              , Inheritance { subClass = "E", superClass = "D" }
              , Inheritance { subClass = "B", superClass = "D" }
              , Composition 
                  { compositionName = "y"
                  , compositionPart = LimitedLinking 
                      { linking = "B", limits = ( 1, Just 1 ) }
                  , compositionWhole = LimitedLinking 
                      { linking = "A", limits = ( 1, Just 1 ) } 
                  }
              , Aggregation 
                  { aggregationName = "x"
                  , aggregationPart = LimitedLinking 
                      { linking = "E", limits = ( 1, Just 2 ) }
                  , aggregationWhole = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 2 ) } 
                  }
              , Association 
                  { associationName = "z"
                  , associationFrom = LimitedLinking 
                      { linking = "C", limits = ( 1, Just 2 ) }
                  , associationTo = LimitedLinking 
                      { linking = "A", limits = ( 1, Nothing ) } 
                  }
              , Association 
                  { associationName = "v"
                  , associationFrom = LimitedLinking 
                      { linking = "C", limits = ( 0, Nothing ) }
                  , associationTo = LimitedLinking 
                      { linking = "B", limits = ( 0, Just 1 ) } 
                  } 
              ] 
            } 
        ) 
      ]
  , instances = fromList
      [ ( 'a'
        , ( [ 1, 2 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
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
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = True
                    , objectName = "a1"
                    , objectClass = "A" 
                    }
                , Object 
                    { isAnonymous = True
                    , objectName = "b1"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "c1"
                    , objectClass = "C" 
                    } 
                ]
              , links = 
                [ Link { linkName = "z", linkFrom = "c1", linkTo = "a1" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c1" }
                , Link { linkName = "x", linkFrom = "e1", linkTo = "c" }
                , Link { linkName = "v", linkFrom = "c1", linkTo = "b1" }
                , Link { linkName = "y", linkFrom = "b", linkTo = "a1" }
                , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                , Link { linkName = "y", linkFrom = "b1", linkTo = "a" }
                , Link { linkName = "z", linkFrom = "c", linkTo = "a" } 
                ] 
              } 
          ) 
        )
      , ( 'b'
        , ( [ 1 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "b"
                    , objectClass = "B" 
                    }
                , Object 
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
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
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
                    , objectName = "e"
                    , objectClass = "E" 
                    } 
                ]
              , links = 
                [ Link { linkName = "y", linkFrom = "b1", linkTo = "a" }
                , Link { linkName = "v", linkFrom = "c", linkTo = "b1" }
                , Link { linkName = "y", linkFrom = "b", linkTo = "a1" }
                , Link { linkName = "w", linkFrom = "e1", linkTo = "b" }
                , Link { linkName = "z", linkFrom = "c", linkTo = "a1" }
                , Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                , Link { linkName = "x", linkFrom = "e1", linkTo = "c" }
                , Link { linkName = "z", linkFrom = "c", linkTo = "a" } 
                ] 
              } 
          ) 
        )
      , ( 'c'
        , ( [ 1, 2 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = True
                    , objectName = "e1"
                    , objectClass = "E" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "a1"
                    , objectClass = "A" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "c1"
                    , objectClass = "C" 
                    }
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
                    { isAnonymous = True, objectName = "a", objectClass = "A" }
                , Object 
                    { isAnonymous = True, objectName = "b", objectClass = "B" }
                , Object 
                    { isAnonymous = False
                    , objectName = "c"
                    , objectClass = "C" 
                    } 
                ]
              , links = 
                [ Link { linkName = "w", linkFrom = "e1", linkTo = "b1" }
                , Link { linkName = "y", linkFrom = "b1", linkTo = "a1" }
                , Link { linkName = "z", linkFrom = "c", linkTo = "a1" }
                , Link { linkName = "v", linkFrom = "c1", linkTo = "b1" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c1" }
                , Link { linkName = "x", linkFrom = "e1", linkTo = "c" }
                , Link { linkName = "z", linkFrom = "c1", linkTo = "a" }
                , Link { linkName = "y", linkFrom = "b", linkTo = "a" } 
                ] 
              } 
          ) 
        )
      , ( 'd'
        , ( [ 2 ]
          , ObjectDiagram 
              { objects = 
                [ Object 
                    { isAnonymous = False
                    , objectName = "c1"
                    , objectClass = "C" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "e"
                    , objectClass = "E" 
                    }
                , Object 
                    { isAnonymous = True
                    , objectName = "b1"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = False
                    , objectName = "a"
                    , objectClass = "A" 
                    }
                , Object 
                    { isAnonymous = True
                    , objectName = "a1"
                    , objectClass = "A" 
                    }
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
                ]
              , links = 
                [ Link { linkName = "z", linkFrom = "c", linkTo = "a1" }
                , Link { linkName = "y", linkFrom = "b", linkTo = "a" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c1" }
                , Link { linkName = "z", linkFrom = "c1", linkTo = "a" }
                , Link { linkName = "y", linkFrom = "b1", linkTo = "a1" }
                , Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c" }
                , Link { linkName = "v", linkFrom = "c1", linkTo = "b1" } 
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
                    , objectName = "b1"
                    , objectClass = "B" 
                    }
                , Object 
                    { isAnonymous = True, objectName = "c", objectClass = "C" }
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
                    , objectName = "c1"
                    , objectClass = "C" 
                    } 
                ]
              , links = 
                [ Link { linkName = "w", linkFrom = "e", linkTo = "b" }
                , Link { linkName = "z", linkFrom = "c", linkTo = "a" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c1" }
                , Link { linkName = "v", linkFrom = "c1", linkTo = "b1" }
                , Link { linkName = "y", linkFrom = "b", linkTo = "a" }
                , Link { linkName = "x", linkFrom = "e", linkTo = "c" }
                , Link { linkName = "y", linkFrom = "b1", linkTo = "a1" }
                , Link { linkName = "z", linkFrom = "c1", linkTo = "a1" } 
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
