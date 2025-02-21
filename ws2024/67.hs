MatchAdInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "H" }
        , AdActionNode { label = 2, name = "G" }
        , AdActionNode { label = 3, name = "A" }
        , AdActionNode { label = 4, name = "D" }
        , AdActionNode { label = 5, name = "I" }
        , AdActionNode { label = 6, name = "F" }
        , AdObjectNode { label = 7, name = "J" }
        , AdObjectNode { label = 8, name = "C" }
        , AdObjectNode { label = 9, name = "B" }
        , AdObjectNode { label = 10, name = "K" }
        , AdObjectNode { label = 11, name = "E" }
        , AdDecisionNode { label = 12 }
        , AdDecisionNode { label = 13 }
        , AdMergeNode { label = 14 }
        , AdMergeNode { label = 15 }
        , AdForkNode { label = 16 }
        , AdJoinNode { label = 17 }
        , AdActivityFinalNode { label = 18 }
        , AdFlowFinalNode { label = 19 }
        , AdInitialNode { label = 20 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 13, guard = "" }
        , AdConnection { from = 2, to = 15, guard = "" }
        , AdConnection { from = 3, to = 4, guard = "" }
        , AdConnection { from = 4, to = 10, guard = "" }
        , AdConnection { from = 5, to = 12, guard = "" }
        , AdConnection { from = 6, to = 17, guard = "" }
        , AdConnection { from = 7, to = 19, guard = "" }
        , AdConnection { from = 8, to = 15, guard = "" }
        , AdConnection { from = 9, to = 5, guard = "" }
        , AdConnection { from = 10, to = 9, guard = "" }
        , AdConnection { from = 11, to = 17, guard = "" }
        , AdConnection { from = 12, to = 2, guard = "b" }
        , AdConnection { from = 12, to = 8, guard = "a" }
        , AdConnection { from = 13, to = 14, guard = "b" }
        , AdConnection { from = 13, to = 18, guard = "c" }
        , AdConnection { from = 14, to = 1, guard = "" }
        , AdConnection { from = 15, to = 16, guard = "" }
        , AdConnection { from = 16, to = 6, guard = "" }
        , AdConnection { from = 16, to = 11, guard = "" }
        , AdConnection { from = 16, to = 14, guard = "" }
        , AdConnection { from = 17, to = 7, guard = "" }
        , AdConnection { from = 20, to = 3, guard = "" } 
        ] 
      }
  , plantUMLConf = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = False }
  , showSolution = True 
  }