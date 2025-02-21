RepairCdInstance 
  { byName = True
  , cdDrawSettings = CdDrawSettings 
      { omittedDefaults = OmittedDefaultMultiplicities 
          { aggregationWholeOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , associationOmittedDefaultMultiplicity = Just ( 0, Nothing )
          , compositionWholeOmittedDefaultMultiplicity = Nothing 
          }
      , printNames = True
      , printNavigations = False 
      }
  , changes = fromList
      [ ( 1
        , InValidOption 
            { hint = Left (AnyClassDiagram 
                { anyClassNames = [ "D", "C", "A", "E", "B" ]
                , anyRelationships = 
                  [ Right (Aggregation 
                      { aggregationName = "y"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 0 ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 1 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "D", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Inheritance { subClass = "D", superClass = "E" })
                  , Right (Association 
                      { associationName = "w"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "s"
                      , aggregationPart = (LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) }) 
                      })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "E", limits = ( 2, Just 2 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) }) 
                      })
                  , Right (Composition 
                      { compositionName = "x"
                      , compositionPart = (LimitedLinking 
                          { linking = "E", limits = ( 2, Just 2 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 1 ) }) 
                      }) 
                  ] 
                })
            , option = Annotation 
                { annotated = Change 
                    { add = Just (Right (Composition 
                        { compositionName = "x"
                        , compositionPart = (LimitedLinking 
                            { linking = "E", limits = ( 2, Just 2 ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "C", limits = ( 1, Just 1 ) }) 
                        }))
                    , remove = Nothing 
                    }
                , annotation = DefiniteArticle 
                } 
            } 
        )
      , ( 2
        , InValidOption 
            { hint = Left (AnyClassDiagram 
                { anyClassNames = [ "B", "D", "E", "A", "C" ]
                , anyRelationships = 
                  [ Right (Composition 
                      { compositionName = "t"
                      , compositionPart = (LimitedLinking 
                          { linking = "A", limits = ( 1, Just 1 ) })
                      , compositionWhole = (LimitedLinking 
                          { linking = "D", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Aggregation 
                      { aggregationName = "s"
                      , aggregationPart = (LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) }) 
                      })
                  , Right (Inheritance { subClass = "E", superClass = "D" })
                  , Right (Aggregation 
                      { aggregationName = "y"
                      , aggregationPart = (LimitedLinking 
                          { linking = "B", limits = ( 0, Nothing ) })
                      , aggregationWhole = (LimitedLinking 
                          { linking = "C", limits = ( 1, Just 0 ) }) 
                      })
                  , Right (Association 
                      { associationName = "w"
                      , associationFrom = (LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) })
                      , associationTo = (LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) }) 
                      })
                  , Right (Association 
                      { associationName = "z"
                      , associationFrom = (LimitedLinking 
                          { linking = "E", limits = ( 2, Just 2 ) })
                      , associationTo = (LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) }) 
                      }) 
                  ] 
                })
            , option = Annotation 
                { annotated = Change 
                    { add = Just (Right (Inheritance 
                        { subClass = "E", superClass = "D" }))
                    , remove = Just (Right (Inheritance 
                        { subClass = "D", superClass = "E" })) 
                    }
                , annotation = DefiniteArticle 
                } 
            } 
        )
      , ( 3
        , InValidOption 
            { hint = Right (ClassDiagram 
                { classNames = [ "D", "E", "B", "C", "A" ]
                , relationships = 
                  [ Association 
                      { associationName = "z"
                      , associationFrom = LimitedLinking 
                          { linking = "E", limits = ( 2, Just 2 ) }
                      , associationTo = LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) } 
                      }
                  , Inheritance { subClass = "D", superClass = "E" }
                  , Association 
                      { associationName = "w"
                      , associationFrom = LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) }
                      , associationTo = LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) } 
                      }
                  , Composition 
                      { compositionName = "v"
                      , compositionPart = LimitedLinking 
                          { linking = "B", limits = ( 0, Nothing ) }
                      , compositionWhole = LimitedLinking 
                          { linking = "C", limits = ( 1, Just 1 ) } 
                      }
                  , Aggregation 
                      { aggregationName = "s"
                      , aggregationPart = LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) }
                      , aggregationWhole = LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) } 
                      }
                  , Composition 
                      { compositionName = "t"
                      , compositionPart = LimitedLinking 
                          { linking = "A", limits = ( 1, Just 1 ) }
                      , compositionWhole = LimitedLinking 
                          { linking = "D", limits = ( 0, Just 1 ) } 
                      } 
                  ] 
                })
            , option = Annotation 
                { annotated = Change 
                    { add = Just (Right (Composition 
                        { compositionName = "v"
                        , compositionPart = (LimitedLinking 
                            { linking = "B", limits = ( 0, Nothing ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "C", limits = ( 1, Just 1 ) }) 
                        }))
                    , remove = Just (Right (Aggregation 
                        { aggregationName = "y"
                        , aggregationPart = (LimitedLinking 
                            { linking = "B", limits = ( 0, Nothing ) })
                        , aggregationWhole = (LimitedLinking 
                            { linking = "C", limits = ( 1, Just 0 ) }) 
                        })) 
                    }
                , annotation = DefiniteArticle 
                } 
            } 
        )
      , ( 4
        , InValidOption 
            { hint = Right (ClassDiagram 
                { classNames = [ "E", "C", "B", "D", "A" ]
                , relationships = 
                  [ Composition 
                      { compositionName = "u"
                      , compositionPart = LimitedLinking 
                          { linking = "B", limits = ( 0, Nothing ) }
                      , compositionWhole = LimitedLinking 
                          { linking = "C", limits = ( 0, Just 1 ) } 
                      }
                  , Association 
                      { associationName = "z"
                      , associationFrom = LimitedLinking 
                          { linking = "E", limits = ( 2, Just 2 ) }
                      , associationTo = LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) } 
                      }
                  , Aggregation 
                      { aggregationName = "s"
                      , aggregationPart = LimitedLinking 
                          { linking = "D", limits = ( 0, Nothing ) }
                      , aggregationWhole = LimitedLinking 
                          { linking = "C", limits = ( 0, Nothing ) } 
                      }
                  , Composition 
                      { compositionName = "t"
                      , compositionPart = LimitedLinking 
                          { linking = "A", limits = ( 1, Just 1 ) }
                      , compositionWhole = LimitedLinking 
                          { linking = "D", limits = ( 0, Just 1 ) } 
                      }
                  , Inheritance { subClass = "D", superClass = "E" }
                  , Association 
                      { associationName = "w"
                      , associationFrom = LimitedLinking 
                          { linking = "A", limits = ( 1, Nothing ) }
                      , associationTo = LimitedLinking 
                          { linking = "B", limits = ( 0, Just 1 ) } 
                      } 
                  ] 
                })
            , option = Annotation 
                { annotated = Change 
                    { add = Just (Right (Composition 
                        { compositionName = "u"
                        , compositionPart = (LimitedLinking 
                            { linking = "B", limits = ( 0, Nothing ) })
                        , compositionWhole = (LimitedLinking 
                            { linking = "C", limits = ( 0, Just 1 ) }) 
                        }))
                    , remove = Just (Right (Aggregation 
                        { aggregationName = "y"
                        , aggregationPart = (LimitedLinking 
                            { linking = "B", limits = ( 0, Nothing ) })
                        , aggregationWhole = (LimitedLinking 
                            { linking = "C", limits = ( 1, Just 0 ) }) 
                        })) 
                    }
                , annotation = DefiniteArticle 
                } 
            } 
        ) 
      ]
  , classDiagram = AnyClassDiagram 
      { anyClassNames = [ "A", "C", "E", "B", "D" ]
      , anyRelationships = 
        [ Right (Composition 
            { compositionName = "t"
            , compositionPart = (LimitedLinking 
                { linking = "A", limits = ( 1, Just 1 ) })
            , compositionWhole = (LimitedLinking 
                { linking = "D", limits = ( 0, Just 1 ) }) 
            })
        , Right (Association 
            { associationName = "z"
            , associationFrom = (LimitedLinking 
                { linking = "E", limits = ( 2, Just 2 ) })
            , associationTo = (LimitedLinking 
                { linking = "A", limits = ( 1, Nothing ) }) 
            })
        , Right (Aggregation 
            { aggregationName = "y"
            , aggregationPart = (LimitedLinking 
                { linking = "B", limits = ( 0, Nothing ) })
            , aggregationWhole = (LimitedLinking 
                { linking = "C", limits = ( 1, Just 0 ) }) 
            })
        , Right (Inheritance { subClass = "D", superClass = "E" })
        , Right (Association 
            { associationName = "w"
            , associationFrom = (LimitedLinking 
                { linking = "A", limits = ( 1, Nothing ) })
            , associationTo = (LimitedLinking 
                { linking = "B", limits = ( 0, Just 1 ) }) 
            })
        , Right (Aggregation 
            { aggregationName = "s"
            , aggregationPart = (LimitedLinking 
                { linking = "D", limits = ( 0, Nothing ) })
            , aggregationWhole = (LimitedLinking 
                { linking = "C", limits = ( 0, Nothing ) }) 
            }) 
        ] 
      }
  , showExtendedFeedback = True
  , showSolution = True
  , taskText = 
    [ Paragraph 
      [ Translated (fromList
          [ ( English
            , "Consider the following class diagram, which unfortunately is invalid:" 
            )
          , ( German
            , "Betrachten Sie folgendes Klassendiagramm, welches leider ung\252ltig ist:" 
            ) 
          ]) 
      ]
    , Paragraph 
      [ Special IncorrectCd ]
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Which of the following changes would each repair the class diagram?" 
            )
          , ( German
            , "Welche der folgenden \196nderungen w\252rden jeweils das Klassendiagramm reparieren?" 
            ) 
          ]) 
      ]
    , Special PotentialFixes
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Please state your answer by giving a list of numbers, indicating all changes each resulting in a valid class diagram." 
            )
          , ( German
            , "Bitte geben Sie Ihre Antwort als Liste aller Zahlen an, deren \196nderungen jeweils in einem g\252ltigen Klassendiagramm resultieren." 
            ) 
          ]) 
      ]
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Answer by giving a comma separated list of all appropriate options, e.g., " 
            )
          , ( German
            , "Antworten Sie durch Angabe einer durch Komma separierten Liste aller zutreffenden Optionen. Zum Beispiel " 
            ) 
          ])
      , Code (fromList
          [ ( English, "[1, 2]" ), ( German, "[1, 2]" ) ])
      , Translated (fromList
          [ ( English
            , " would indicate that options 1 and 2 each repair the given class diagram." 
            )
          , ( German
            , " als Angabe w\252rde bedeuten, dass die Optionen 1 und 2 jeweils das gegebene Klassendiagramm reparieren." 
            ) 
          ]) 
      ] 
    ] 
  }
