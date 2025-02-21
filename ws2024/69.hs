EnterASInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "H" }
        , AdActionNode { label = 2, name = "K" }
        , AdActionNode { label = 3, name = "A" }
        , AdActionNode { label = 4, name = "N" }
        , AdActionNode { label = 5, name = "C" }
        , AdActionNode { label = 6, name = "D" }
        , AdActionNode { label = 7, name = "S" }
        , AdActionNode { label = 8, name = "Q" }
        , AdActionNode { label = 9, name = "P" }
        , AdActionNode { label = 10, name = "E" }
        , AdActionNode { label = 11, name = "I" }
        , AdActionNode { label = 12, name = "O" }
        , AdActionNode { label = 13, name = "R" }
        , AdActionNode { label = 14, name = "B" }
        , AdObjectNode { label = 15, name = "J" }
        , AdObjectNode { label = 16, name = "M" }
        , AdObjectNode { label = 17, name = "L" }
        , AdObjectNode { label = 18, name = "T" }
        , AdObjectNode { label = 19, name = "F" }
        , AdObjectNode { label = 20, name = "G" }
        , AdDecisionNode { label = 21 }
        , AdMergeNode { label = 22 }
        , AdForkNode { label = 23 }
        , AdForkNode { label = 24 }
        , AdJoinNode { label = 25 }
        , AdJoinNode { label = 26 }
        , AdFlowFinalNode { label = 27 }
        , AdFlowFinalNode { label = 28 }
        , AdInitialNode { label = 29 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 26, guard = "" }
        , AdConnection { from = 2, to = 18, guard = "" }
        , AdConnection { from = 3, to = 28, guard = "" }
        , AdConnection { from = 4, to = 25, guard = "" }
        , AdConnection { from = 5, to = 20, guard = "" }
        , AdConnection { from = 6, to = 21, guard = "" }
        , AdConnection { from = 7, to = 26, guard = "" }
        , AdConnection { from = 8, to = 10, guard = "" }
        , AdConnection { from = 9, to = 25, guard = "" }
        , AdConnection { from = 10, to = 22, guard = "" }
        , AdConnection { from = 11, to = 23, guard = "" }
        , AdConnection { from = 12, to = 19, guard = "" }
        , AdConnection { from = 13, to = 8, guard = "" }
        , AdConnection { from = 14, to = 24, guard = "" }
        , AdConnection { from = 15, to = 1, guard = "" }
        , AdConnection { from = 16, to = 15, guard = "" }
        , AdConnection { from = 17, to = 16, guard = "" }
        , AdConnection { from = 18, to = 25, guard = "" }
        , AdConnection { from = 19, to = 7, guard = "" }
        , AdConnection { from = 20, to = 27, guard = "" }
        , AdConnection { from = 21, to = 11, guard = "b" }
        , AdConnection { from = 21, to = 22, guard = "a" }
        , AdConnection { from = 22, to = 6, guard = "" }
        , AdConnection { from = 23, to = 5, guard = "" }
        , AdConnection { from = 23, to = 12, guard = "" }
        , AdConnection { from = 23, to = 17, guard = "" }
        , AdConnection { from = 24, to = 2, guard = "" }
        , AdConnection { from = 24, to = 4, guard = "" }
        , AdConnection { from = 24, to = 9, guard = "" }
        , AdConnection { from = 25, to = 3, guard = "" }
        , AdConnection { from = 26, to = 14, guard = "" }
        , AdConnection { from = 29, to = 13, guard = "" } 
        ] 
      }
  , drawSettings = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = True }
  , sampleSequence = 
    [ "R", "Q", "E", "D", "I", "H", "O", "S", "B", "P", "N", "K", "C", "A" ]
  , showSolution = True 
  }