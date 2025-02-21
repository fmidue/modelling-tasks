NameCdErrorInstance 
  { byName = True
  , classDiagram = AnnotatedClassDiagram 
      { annotatedClasses = [ "E", "A", "C", "D", "B" ]
      , annotatedRelationships = 
        [ Annotation 
            { annotated = Right (Association 
                { associationName = "z"
                , associationFrom = (LimitedLinking 
                    { linking = "E", limits = ( 2, Nothing ) })
                , associationTo = (LimitedLinking 
                    { linking = "D", limits = ( 0, Nothing ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 6
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Left (InvalidInheritance 
                { invalidSubClass = (LimitedLinking 
                    { linking = "A", limits = ( 0, Nothing ) })
                , invalidSuperClass = (LimitedLinking 
                    { linking = "C", limits = ( 0, Nothing ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = True
                , listingPriority = 5
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Right (Composition 
                { compositionName = "u"
                , compositionPart = (LimitedLinking 
                    { linking = "B", limits = ( 2, Nothing ) })
                , compositionWhole = (LimitedLinking 
                    { linking = "E", limits = ( 1, Just 1 ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 4
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Right (Aggregation 
                { aggregationName = "y"
                , aggregationPart = (LimitedLinking 
                    { linking = "D", limits = ( 2, Just 2 ) })
                , aggregationWhole = (LimitedLinking 
                    { linking = "C", limits = ( 2, Just 2 ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 3
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Right (Aggregation 
                { aggregationName = "w"
                , aggregationPart = (LimitedLinking 
                    { linking = "E", limits = ( 2, Nothing ) })
                , aggregationWhole = (LimitedLinking 
                    { linking = "D", limits = ( 0, Just 1 ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 2
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Right (Composition 
                { compositionName = "x"
                , compositionPart = (LimitedLinking 
                    { linking = "D", limits = ( 1, Just 1 ) })
                , compositionWhole = (LimitedLinking 
                    { linking = "B", limits = ( 1, Just 1 ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 1
                , referenceUsing = DefiniteArticle 
                } 
            }
        , Annotation 
            { annotated = Right (Association 
                { associationName = "v"
                , associationFrom = (LimitedLinking 
                    { linking = "B", limits = ( 1, Nothing ) })
                , associationTo = (LimitedLinking 
                    { linking = "E", limits = ( 0, Just 2 ) }) 
                })
            , annotation = Relevant 
                { contributingToProblem = False
                , listingPriority = 7
                , referenceUsing = DefiniteArticle 
                } 
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
      , printNavigations = False 
      }
  , errorReasons = fromList
      [ ( 'a', ( False, PreDefined ReverseRelationships ) )
      , ( 'b', ( False, PreDefined ReverseInheritances ) )
      , ( 'c', ( False, PreDefined SelfInheritances ) )
      , ( 'd', ( False, PreDefined InheritanceCycles ) )
      , ( 'e', ( False, PreDefined WrongCompositionLimits ) )
      , ( 'f', ( False, PreDefined SelfRelationships ) )
      , ( 'g', ( True, PreDefined InvalidInheritanceLimits ) )
      , ( 'h', ( False, PreDefined CompositionCycles ) )
      , ( 'i', ( False, PreDefined MultipleInheritances ) )
      , ( 'j', ( False, PreDefined DoubleRelationships ) ) 
      ]
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
            , "It contains the following relationships between classes:" 
            )
          , ( German
            , "Es enth\228lt die folgenden Beziehungen zwischen Klassen:" 
            ) 
          ]) 
      ]
    , Paragraph 
      [ Special RelationshipsList ]
    , Paragraph 
      [ Translated (fromList
          [ ( English
            , "Choose what you think is the single reason that this class diagram is incorrect, and mention all relationships that definitely contribute to the problem, i.e., removing any of them would fix the problem." 
            )
          , ( German
            , "W\228hlen Sie aus, was Sie f\252r den einen Grund daf\252r halten, dass dieses Klassendiagramm ung\252ltig ist, und nennen Sie alle Beziehungen, die definitiv zum Problem beitragen, d.h., deren Entfernung das Problem jeweils beheben w\252rde." 
            ) 
          ]) 
      ]
    , Paragraph 
      [ Translated (fromList
          [ ( English, "Reasons available to choose from are:" )
          , ( German, "Gr\252nde, die hierf\252r zur Auswahl stehen, sind:" ) 
          ]) 
      ]
    , Paragraph 
      [ Translated (fromList
          [ ( English, "The class diagram ..." )
          , ( German, "Das Klassendiagramm ..." ) 
          ]) 
      ]
    , Paragraph 
      [ Special ReasonsList ]
    , Paragraph 
      [ Paragraph 
        [ Translated (fromList
            [ ( English
              , "Please state your answer by providing a letter for the reason, indicating the most specifically expressed reason for which you think this class diagram is invalid, and a listing of numbers for those relationships on whose individual presence the problem depends. For example," 
              )
            , ( German
              , "Bitte geben Sie Ihre Antwort an, indem Sie Folgendes angeben: einen Buchstaben f\252r den Grund, der Ihrer Meinung nach der am spezifischsten ausgedr\252ckte Grund daf\252r ist, dass dieses Klassendiagramm ung\252ltig ist, und eine Auflistung von Zahlen f\252r diejenigen Beziehungen, von deren individueller Pr\228senz das Problem abh\228ngt. Zum Beispiel w\252rde" 
              ) 
            ]) 
        ]
      , Paragraph 
        [ Code (fromList
            [ ( English, "due-to:\n- 3\n- 4\nreason: b\n" )
            , ( German, "due-to:\n- 3\n- 4\nreason: b\n" ) 
            ]) 
        ]
      , Paragraph 
        [ Translated (fromList
            [ ( English
              , "would indicate that the class diagram is invalid because of reason b and that the 3. and 4. relationship (appearing together) create the problem." 
              )
            , ( German
              , "bedeuten, dass das Klassendiagramm wegen Grund b ung\252ltig ist und dass die 3. und 4. Beziehung (zusammen auftretend) das Problem erzeugen." 
              ) 
            ]) 
        ] 
      ] 
    ] 
  }
