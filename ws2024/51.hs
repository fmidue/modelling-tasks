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
                    , remove = (Just (Right (Inheritance 
                        { subClass = "E", superClass = "B" }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "A", "B", "D", "C", "E" ]
                , anyRelationships = 
                  [ Right (Inheritance { subClass = "E", superClass = "B" })
                  , Right (Composition 
                      { compositionName = "s"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 2, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "w"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 0, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "v"
                      , aggregationPart = (LimitedLinking 
                          { linking = "D", limits = ( 1, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 2, Just 2 ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "C" })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      }) 
                  ] 
                } 
            } 
        )
      , ( 2
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
                      , objectName = "c"
                      , objectClass = "C" 
                      }
                  , Object 
                      { isAnonymous = False
                      , objectName = "e"
                      , objectClass = "E" 
                      }
                  , Object 
                      { isAnonymous = True
                      , objectName = "d"
                      , objectClass = "D" 
                      }
                  , Object 
                      { isAnonymous = False
                      , objectName = "b"
                      , objectClass = "B" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "y", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "s", linkFrom = "c", linkTo = "e" }
                  , Link { linkName = "t", linkFrom = "a", linkTo = "e" }
                  , Link { linkName = "s", linkFrom = "b", linkTo = "e" }
                  , Link { linkName = "z", linkFrom = "a", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "b", linkTo = "a" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "C", "D", "B", "E", "A" ]
                , anyRelationships = 
                  [ Right (Aggregation 
                      { aggregationName = "y"
                      , aggregationPart = (LimitedLinking 
                          { linking = "E", limits = ( 1, Just 1 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "C" })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "s"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 2, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "w"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 0, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
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
                    , remove = (Just (Right (Inheritance 
                        { subClass = "E", superClass = "B" }))) 
                    })
                , annotation = DefiniteArticle 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "D", "C", "B", "E", "A" ]
                , anyRelationships = 
                  [ Right (Aggregation 
                      { aggregationName = "u"
                      , aggregationPart = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "D", limits = ( 0, Just 2 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "s"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 2, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 0, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "E", superClass = "B" })
                  , Right (Inheritance { subClass = "B", superClass = "C" })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "w"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 2 ) }) 
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
                      , objectName = "b"
                      , objectClass = "B" 
                      }
                  , Object 
                      { isAnonymous = True
                      , objectName = "e"
                      , objectClass = "E" 
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
                      { isAnonymous = False
                      , objectName = "c"
                      , objectClass = "C" 
                      } 
                  ]
                , links = 
                  [ Link { linkName = "z", linkFrom = "a", linkTo = "b" }
                  , Link { linkName = "w", linkFrom = "b", linkTo = "a" }
                  , Link { linkName = "x", linkFrom = "e", linkTo = "b" }
                  , Link { linkName = "s", linkFrom = "c", linkTo = "e" }
                  , Link { linkName = "s", linkFrom = "b", linkTo = "e" }
                  , Link { linkName = "t", linkFrom = "a", linkTo = "e" } 
                  ] 
                })
            , option = AnyClassDiagram 
                { anyClassNames = [ "D", "E", "B", "A", "C" ]
                , anyRelationships = 
                  [ Right (Composition 
                      { compositionName = "s"
                      , compositionPart = (LimitedLinking 
                          { linking = "C", limits = ( 2, Nothing ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "B", superClass = "C" })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 0, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "x"
                      , aggregationPart = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 2 ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "E", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "w"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 1, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "A", limits = ( 0, Just 2 ) }) 
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
