MatchPetriInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "A" }
        , AdActionNode { label = 2, name = "C" }
        , AdActionNode { label = 3, name = "B" }
        , AdActionNode { label = 4, name = "I" }
        , AdActionNode { label = 5, name = "E" }
        , AdActionNode { label = 6, name = "F" }
        , AdActionNode { label = 7, name = "M" }
        , AdActionNode { label = 8, name = "G" }
        , AdObjectNode { label = 9, name = "K" }
        , AdObjectNode { label = 10, name = "L" }
        , AdObjectNode { label = 11, name = "J" }
        , AdObjectNode { label = 12, name = "D" }
        , AdObjectNode { label = 13, name = "H" }
        , AdDecisionNode { label = 14 }
        , AdDecisionNode { label = 15 }
        , AdDecisionNode { label = 16 }
        , AdMergeNode { label = 17 }
        , AdMergeNode { label = 18 }
        , AdMergeNode { label = 19 }
        , AdForkNode { label = 20 }
        , AdForkNode { label = 21 }
        , AdJoinNode { label = 22 }
        , AdJoinNode { label = 23 }
        , AdFlowFinalNode { label = 24 }
        , AdFlowFinalNode { label = 25 }
        , AdFlowFinalNode { label = 26 }
        , AdInitialNode { label = 27 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 23, guard = "" }
        , AdConnection { from = 2, to = 22, guard = "" }
        , AdConnection { from = 3, to = 16, guard = "" }
        , AdConnection { from = 4, to = 23, guard = "" }
        , AdConnection { from = 5, to = 12, guard = "" }
        , AdConnection { from = 6, to = 24, guard = "" }
        , AdConnection { from = 7, to = 8, guard = "" }
        , AdConnection { from = 8, to = 19, guard = "" }
        , AdConnection { from = 9, to = 13, guard = "" }
        , AdConnection { from = 10, to = 22, guard = "" }
        , AdConnection { from = 11, to = 15, guard = "" }
        , AdConnection { from = 12, to = 7, guard = "" }
        , AdConnection { from = 13, to = 25, guard = "" }
        , AdConnection { from = 14, to = 9, guard = "a" }
        , AdConnection { from = 14, to = 19, guard = "b" }
        , AdConnection { from = 15, to = 4, guard = "b" }
        , AdConnection { from = 15, to = 17, guard = "c" }
        , AdConnection { from = 16, to = 18, guard = "c" }
        , AdConnection { from = 16, to = 26, guard = "d" }
        , AdConnection { from = 17, to = 11, guard = "" }
        , AdConnection { from = 18, to = 20, guard = "" }
        , AdConnection { from = 19, to = 21, guard = "" }
        , AdConnection { from = 20, to = 2, guard = "" }
        , AdConnection { from = 20, to = 6, guard = "" }
        , AdConnection { from = 20, to = 10, guard = "" }
        , AdConnection { from = 21, to = 1, guard = "" }
        , AdConnection { from = 21, to = 17, guard = "" }
        , AdConnection { from = 21, to = 18, guard = "" }
        , AdConnection { from = 22, to = 3, guard = "" }
        , AdConnection { from = 23, to = 14, guard = "" }
        , AdConnection { from = 27, to = 5, guard = "" } 
        ] 
      }
  , petriNet = PetriLike 
      { allNodes = fromList
          [ ( NormalPetriNode 
                { label = 1
                , sourceNode = AdObjectNode { label = 13, name = "H" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( FinalPetriNode 
                          { label = 27
                          , sourceNode = AdFlowFinalNode { label = 25 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 2 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 26
                          , sourceNode = AdJoinNode { label = 23 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 3, sourceNode = AdDecisionNode { label = 15 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 14
                          , sourceNode = AdActionNode { label = 4, name = "I" } 
                          }
                      , 1 
                      )
                    , ( AuxiliaryPetriNode { label = 33 }, 1 ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 4 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 25
                          , sourceNode = AdActionNode { label = 1, name = "A" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 5 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 3
                          , sourceNode = AdDecisionNode { label = 15 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 6, sourceNode = AdMergeNode { label = 18 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 38
                          , sourceNode = AdForkNode { label = 20 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 7 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 1
                          , sourceNode = AdObjectNode 
                              { label = 13, name = "H" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( FinalPetriNode 
                { label = 8, sourceNode = AdFlowFinalNode { label = 26 } }
            , SimpleTransition { flowOut = fromList [ ] }
            )
          , ( AuxiliaryPetriNode { label = 9 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 12
                          , sourceNode = AdActionNode { label = 2, name = "C" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 10, sourceNode = AdForkNode { label = 21 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 4 }, 1 )
                    , ( NormalPetriNode 
                          { label = 6
                          , sourceNode = AdMergeNode { label = 18 } 
                          }
                      , 1 
                      )
                    , ( NormalPetriNode 
                          { label = 19
                          , sourceNode = AdMergeNode { label = 17 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 11, sourceNode = AdInitialNode { label = 27 } }
            , SimplePlace 
                { initial = 1
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 41
                          , sourceNode = AdActionNode { label = 5, name = "E" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 12
                , sourceNode = AdActionNode { label = 2, name = "C" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 28 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 13, sourceNode = AdDecisionNode { label = 14 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 16 }, 1 )
                    , ( AuxiliaryPetriNode { label = 24 }, 1 ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 14
                , sourceNode = AdActionNode { label = 4, name = "I" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 2 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 15
                , sourceNode = AdObjectNode { label = 9, name = "K" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 7 }, 1 ) ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 16 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 15
                          , sourceNode = AdObjectNode { label = 9, name = "K" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 17
                , sourceNode = AdObjectNode { label = 10, name = "L" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 30
                          , sourceNode = AdJoinNode { label = 22 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 18, sourceNode = AdMergeNode { label = 19 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 10
                          , sourceNode = AdForkNode { label = 21 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 19, sourceNode = AdMergeNode { label = 17 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 29 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 20
                , sourceNode = AdActionNode { label = 3, name = "B" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 32
                          , sourceNode = AdDecisionNode { label = 16 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 21
                , sourceNode = AdObjectNode { label = 12, name = "D" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 23
                          , sourceNode = AdActionNode { label = 7, name = "M" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 22
                , sourceNode = AdObjectNode { label = 11, name = "J" } 
                }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 5 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 23
                , sourceNode = AdActionNode { label = 7, name = "M" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 39 }, 1 ) ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 24 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 18
                          , sourceNode = AdMergeNode { label = 19 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 25
                , sourceNode = AdActionNode { label = 1, name = "A" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 37 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 26, sourceNode = AdJoinNode { label = 23 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 13
                          , sourceNode = AdDecisionNode { label = 14 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( FinalPetriNode 
                { label = 27, sourceNode = AdFlowFinalNode { label = 25 } }
            , SimpleTransition { flowOut = fromList [ ] }
            )
          , ( AuxiliaryPetriNode { label = 28 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 30
                          , sourceNode = AdJoinNode { label = 22 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 29 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 22
                          , sourceNode = AdObjectNode 
                              { label = 11, name = "J" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 30, sourceNode = AdJoinNode { label = 22 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 40 }, 1 ) ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 31
                , sourceNode = AdActionNode { label = 8, name = "G" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 18
                          , sourceNode = AdMergeNode { label = 19 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 32, sourceNode = AdDecisionNode { label = 16 } }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( FinalPetriNode 
                          { label = 8
                          , sourceNode = AdFlowFinalNode { label = 26 } 
                          }
                      , 1 
                      )
                    , ( AuxiliaryPetriNode { label = 34 }, 1 ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 33 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 19
                          , sourceNode = AdMergeNode { label = 17 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 34 }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 6
                          , sourceNode = AdMergeNode { label = 18 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 35
                , sourceNode = AdActionNode { label = 6, name = "F" } 
                }
            , SimpleTransition { flowOut = fromList [ ] }
            )
          , ( AuxiliaryPetriNode { label = 36 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 35
                          , sourceNode = AdActionNode { label = 6, name = "F" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 37 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 26
                          , sourceNode = AdJoinNode { label = 23 } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 38, sourceNode = AdForkNode { label = 20 } }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( AuxiliaryPetriNode { label = 9 }, 1 )
                    , ( NormalPetriNode 
                          { label = 17
                          , sourceNode = AdObjectNode 
                              { label = 10, name = "L" } 
                          }
                      , 1 
                      )
                    , ( AuxiliaryPetriNode { label = 36 }, 1 ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 39 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 31
                          , sourceNode = AdActionNode { label = 8, name = "G" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( AuxiliaryPetriNode { label = 40 }
            , SimplePlace 
                { initial = 0
                , flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 20
                          , sourceNode = AdActionNode { label = 3, name = "B" } 
                          }
                      , 1 
                      ) 
                    ] 
                } 
            )
          , ( NormalPetriNode 
                { label = 41
                , sourceNode = AdActionNode { label = 5, name = "E" } 
                }
            , SimpleTransition 
                { flowOut = fromList
                    [ ( NormalPetriNode 
                          { label = 21
                          , sourceNode = AdObjectNode 
                              { label = 12, name = "D" } 
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
