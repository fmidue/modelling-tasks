SelectValidCdInstance 
  { cdDrawSettings = CdDrawSettings 
      { omittedDefaults = OmittedDefaultMultiplicities 
          { aggregationWholeOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , associationOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , compositionWholeOmittedDefaultMultiplicity = Nothing 
          }
      , printNames = False
      , printNavigations = False 
      }
  , classDiagrams = fromList
      [ ( 1
        , InValidOption 
            { hint = Right (ObjectDiagram 
                { objects = 
                  [ Object 
                      { isAnonymous = False
                      , objectName = "d"
                      , objectClass = "D" 
                      }
                  , Object 
                      { isAnonymous = False
                      , objectName = "a"
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
                  , Object 
                      { isAnonymous = False
                      , objectName = "e"
                      , objectClass = "E" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "z", linkFrom = "a", linkTo = "b" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "e" }
                  , Link { linkName = "v", linkFrom = "a", linkTo = "c" }
                  , Link { linkName = "t", linkFrom = "d", linkTo = "b" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "c", linkTo = "b" }
                  , Link { linkName = "s", linkFrom = "c", linkTo = "b" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "B", "E", "C", "A", "D" ]
                , anyRelationships = 
                  [ Right (Association 
                      { associationName = "s"
                      , associationFrom = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "v"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "z"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "E" })
                  , Right (Composition 
                      { compositionName = "w"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Association 
                      { associationName = "x"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 2 ) }) 
                      }) 
                  ] 
                } 
            } 
        )
      , ( 2
        , InValidOption 
            { hint = Left (Annotation 
                { annotated = (Change 
                    { add = Nothing
                    , remove = (Just (Right (Composition 
                        { compositionName = "q"
                        , compositionPart = (LimitedLinking 
                            { linking = "D", limits = ( 1, Just 2 ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "B", limits = ( 1, Just 2 ) }) 
                        }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "D", "B", "C", "A", "E" ]
                , anyRelationships = 
                  [ Right (Composition 
                      { compositionName = "q"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "u"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "E" })
                  , Right (Association 
                      { associationName = "s"
                      , associationFrom = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Association 
                      { associationName = "x"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "v"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "z"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      }) 
                  ] 
                } 
            } 
        )
      , ( 3
        , InValidOption 
            { hint = Left (Annotation 
                { annotated = (Change 
                    { add = Nothing
                    , remove = (Just (Right (Composition 
                        { compositionName = "q"
                        , compositionPart = (LimitedLinking 
                            { linking = "D", limits = ( 1, Just 2 ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "B", limits = ( 1, Just 2 ) }) 
                        }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "B", "C", "E", "A", "D" ]
                , anyRelationships = 
                  [ Right (Inheritance { subClass = "B", superClass = "E" })
                  , Right (Composition 
                      { compositionName = "q"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Association 
                      { associationName = "x"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Association 
                      { associationName = "s"
                      , associationFrom = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "w"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "z"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "y"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      }) 
                  ] 
                } 
            } 
        )
      , ( 4
        , InValidOption 
            { hint = Right (ObjectDiagram 
                { objects = 
                  [ Object 
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
                      , objectName = "e"
                      , objectClass = "E" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "v", linkFrom = "a", linkTo = "c" }
                  , Link { linkName = "r", linkFrom = "d", linkTo = "b" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "b" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "e" }
                  , Link { linkName = "s", linkFrom = "c", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "c", linkTo = "b" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "b" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "E", "B", "D", "C", "A" ]
                , anyRelationships = 
                  [ Right (Association 
                      { associationName = "x"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "v"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "E" })
                  , Right (Composition 
                      { compositionName = "r"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Association 
                      { associationName = "s"
                      , associationFrom = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "z"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "w"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      }) 
                  ] 
                } 
            } 
        ) 
      ]
  , showExtendedFeedback = True
  , showSolution = True
  , taskText = 
    [ Paragraph 
      [ Translated (fromList
          [ ( English, "Consider the following class diagram candidates:" )
          , ( German
            , "Betrachten Sie die folgenden Klassendiagrammkandidaten:" 
            ) 
          ]) 
      ]
    , Special CdCandidates
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Which of these class diagram candidates are valid class diagrams?\nPlease state your answer by giving a list of numbers, indicating all valid class diagrams." 
            )
          , ( German
            , "Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?\nBitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle g\252ltigen Klassendiagramme enth\228lt." 
            ) 
          ]) 
      ]
    , Paragraph 
      [ Translated (fromList
          [ ( English, "For example," ), ( German, "Zum Beispiel w\252rde" ) ])
      , Code (fromList
          [ ( English, "[1, 2]" ), ( German, "[1, 2]" ) ])
      , Translated (fromList
          [ ( English
            , "would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams." 
            )
          , ( German
            , "bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten g\252ltige Klassendiagramme sind." 
            ) 
          ]) 
      ] 
    ] 
  }
