MatchPetriInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "F" }
        , AdActionNode { label = 2, name = "B" }
        , AdActionNode { label = 3, name = "A" }
        , AdActionNode { label = 4, name = "G" }
        , AdActionNode { label = 5, name = "L" }
        , AdActionNode { label = 6, name = "C" }
        , AdObjectNode { label = 7, name = "K" }
        , AdObjectNode { label = 8, name = "M" }
        , AdObjectNode { label = 9, name = "J" }
        , AdObjectNode { label = 10, name = "I" }
        , AdObjectNode { label = 11, name = "H" }
        , AdObjectNode { label = 12, name = "D" }
        , AdObjectNode { label = 13, name = "E" }
        , AdDecisionNode { label = 14 }
        , AdMergeNode { label = 15 }
        , AdForkNode { label = 16 }
        , AdJoinNode { label = 17 }
        , AdFlowFinalNode { label = 18 }
        , AdInitialNode { label = 19 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 15, guard = "" }
        , AdConnection { from = 2, to = 9, guard = "" }
        , AdConnection { from = 3, to = 7, guard = "" }
        , AdConnection { from = 4, to = 8, guard = "" }
        , AdConnection { from = 5, to = 15, guard = "" }
        , AdConnection { from = 6, to = 10, guard = "" }
        , AdConnection { from = 7, to = 5, guard = "" }
        , AdConnection { from = 8, to = 17, guard = "" }
        , AdConnection { from = 9, to = 6, guard = "" }
        , AdConnection { from = 10, to = 17, guard = "" }
        , AdConnection { from = 11, to = 4, guard = "" }
        , AdConnection { from = 12, to = 17, guard = "" }
        , AdConnection { from = 13, to = 18, guard = "" }
        , AdConnection { from = 14, to = 1, guard = "a" }
        , AdConnection { from = 14, to = 3, guard = "b" }
        , AdConnection { from = 15, to = 2, guard = "" }
        , AdConnection { from = 16, to = 11, guard = "" }
        , AdConnection { from = 16, to = 12, guard = "" }
        , AdConnection { from = 16, to = 14, guard = "" }
        , AdConnection { from = 17, to = 13, guard = "" }
        , AdConnection { from = 19, to = 16, guard = "" } 
        ] 
      }
  , petriNet = PetriLike 
      { allNodes = fromList
          [ ( NormalPetriNode 
                { label = 1
                , sourceNode = AdActionNode { label = 6, name = "C" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 10
                          , sourceNode = AdObjectNode 
                              { label = 10, name = "I" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( FinalPetriNode 
                { label = 2, sourceNode = AdFlowFinalNode { label = 18 } }
            , SimpleTransition { flowOut = fromList [ ] }
            )
          , ( NormalPetriNode 
                { label = 3, sourceNode = AdJoinNode { label = 17 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 5
                          , sourceNode = AdObjectNode 
                              { label = 13, name = "E" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 4
                , sourceNode = AdActionNode { label = 5, name = "L" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 15
                          , sourceNode = AdMergeNode { label = 15 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 5
                , sourceNode = AdObjectNode { label = 13, name = "E" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( FinalPetriNode 
                          { label = 2
                          , sourceNode = AdFlowFinalNode { label = 18 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 6
                , sourceNode = AdObjectNode { label = 12, name = "D" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 3, sourceNode = AdJoinNode { label = 17 } }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 7, sourceNode = AdDecisionNode { label = 14 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 16
                          , sourceNode = AdActionNode { label = 1, name = "F" } 
                          }
                      , 1 
                      )
                    , ( NormalPetriNode 
                          { label = 18
                          , sourceNode = AdActionNode { label = 3, name = "A" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 8, sourceNode = AdInitialNode { label = 19 } }
            , SimplePlace 
                { initial = 1
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 12
                          , sourceNode = AdForkNode { label = 16 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 9
                , sourceNode = AdObjectNode { label = 8, name = "M" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 3, sourceNode = AdJoinNode { label = 17 } }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 10
                , sourceNode = AdObjectNode { label = 10, name = "I" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 3, sourceNode = AdJoinNode { label = 17 } }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 11
                , sourceNode = AdActionNode { label = 2, name = "B" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 19
                          , sourceNode = AdObjectNode { label = 9, name = "J" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 12, sourceNode = AdForkNode { label = 16 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 6
                          , sourceNode = AdObjectNode 
                              { label = 12, name = "D" } 
                          }
                      , 1 
                      )
                    , ( NormalPetriNode 
                          { label = 7
                          , sourceNode = AdDecisionNode { label = 14 } 
                          }
                      , 1 
                      )
                    , ( NormalPetriNode 
                          { label = 17
                          , sourceNode = AdObjectNode 
                              { label = 11, name = "H" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 13
                , sourceNode = AdActionNode { label = 4, name = "G" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 9
                          , sourceNode = AdObjectNode { label = 8, name = "M" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 14
                , sourceNode = AdObjectNode { label = 7, name = "K" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 4
                          , sourceNode = AdActionNode { label = 5, name = "L" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 15, sourceNode = AdMergeNode { label = 15 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 11
                          , sourceNode = AdActionNode { label = 2, name = "B" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 16
                , sourceNode = AdActionNode { label = 1, name = "F" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 15
                          , sourceNode = AdMergeNode { label = 15 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 17
                , sourceNode = AdObjectNode { label = 11, name = "H" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 13
                          , sourceNode = AdActionNode { label = 4, name = "G" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 18
                , sourceNode = AdActionNode { label = 3, name = "A" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 14
                          , sourceNode = AdObjectNode { label = 7, name = "K" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 19
                , sourceNode = AdObjectNode { label = 9, name = "J" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 1
                          , sourceNode = AdActionNode { label = 6, name = "C" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            ) 
          ] 
      }
  , plantUMLConf = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = True }
  , petriDrawConf = DrawSettings 
      { withPlaceNames = True
      , withSvgHighlighting = True
      , withTransitionNames = True
      , with1Weights = False
      , withGraphvizCommand = Fdp 
      }
  , showSolution = True 
  }
