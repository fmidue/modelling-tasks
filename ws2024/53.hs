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
            { hint = Left (Annotation 
                { annotated = (Change 
                    { add = Nothing
                    , remove = (Just (Right (Composition 
                        { compositionName = "u"
                        , compositionPart = (LimitedLinking 
                            { linking = "E", limits = ( 1, Nothing ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "B", limits = ( 1, Just 1 ) }) 
                        }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "E", "B", "C", "D", "A" ]
                , anyRelationships = 
                  [ Right (Composition 
                      { compositionName = "u"
                      , compositionPart = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "w"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "t"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Inheritance { subClass = "D", superClass = "A" })
                  , Right (Composition 
                      { compositionName = "y"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "E", superClass = "C" })
                  , Right (Association 
                      { associationName = "v"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
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
                        { compositionName = "u"
                        , compositionPart = (LimitedLinking 
                            { linking = "E", limits = ( 1, Nothing ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "B", limits = ( 1, Just 2 ) }) 
                        }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "A", "C", "E", "D", "B" ]
                , anyRelationships = 
                  [ Right (Aggregation 
                      { aggregationName = "t"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "x"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Association 
                      { associationName = "r"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 2, Nothing ) }) 
                      })
                  , Right (Inheritance { subClass = "D", superClass = "A" })
                  , Right (Inheritance { subClass = "E", superClass = "C" })
                  , Right (Composition 
                      { compositionName = "u"
                      , compositionPart = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "y"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
                      }) 
                  ] 
                } 
            } 
        )
      , ( 3
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
                      , objectName = "e"
                      , objectClass = "E" 
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
                      , objectName = "a"
                      , objectClass = "A" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "v", linkFrom = "d", linkTo = "b" }
                  , Link { linkName = "t", linkFrom = "b", linkTo = "d" }
                  , Link { linkName = "y", linkFrom = "c", linkTo = "d" }
                  , Link { linkName = "x", linkFrom = "a", linkTo = "e" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "c" }
                  , Link { linkName = "z", linkFrom = "e", linkTo = "b" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "D", "A", "E", "B", "C" ]
                , anyRelationships = 
                  [ Right (Aggregation 
                      { aggregationName = "t"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Inheritance { subClass = "D", superClass = "A" })
                  , Right (Composition 
                      { compositionName = "z"
                      , compositionPart = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Association 
                      { associationName = "v"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "x"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "y"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "D", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "E", superClass = "C" }) 
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
                      , objectName = "e"
                      , objectClass = "E" 
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
                      , objectName = "a"
                      , objectClass = "A" 
                      }
                  , Object 
                      { isAnonymous = False
                      , objectName = "d"
                      , objectClass = "D" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "x", linkFrom = "a", linkTo = "e" }
                  , Link { linkName = "t", linkFrom = "b", linkTo = "d" }
                  , Link { linkName = "v", linkFrom = "d", linkTo = "b" }
                  , Link { linkName = "y", linkFrom = "d", linkTo = "c" }
                  , Link { linkName = "x", linkFrom = "d", linkTo = "c" }
                  , Link { linkName = "s", linkFrom = "e", linkTo = "b" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "B", "C", "D", "E", "A" ]
                , anyRelationships = 
                  [ Right (Composition 
                      { compositionName = "s"
                      , compositionPart = (LimitedLinking 
                          { linking = "E", limits = ( 1, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "D", superClass = "A" })
                  , Right (Aggregation 
                      { aggregationName = "x"
                      , aggregationPart = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "y"
                      , compositionPart = (LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "E", superClass = "C" })
                  , Right (Association 
                      { associationName = "v"
                      , associationFrom = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "t"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) }) 
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
