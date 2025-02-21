EnterASInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "M" }
        , AdActionNode { label = 2, name = "K" }
        , AdActionNode { label = 3, name = "G" }
        , AdActionNode { label = 4, name = "I" }
        , AdActionNode { label = 5, name = "A" }
        , AdActionNode { label = 6, name = "Q" }
        , AdActionNode { label = 7, name = "P" }
        , AdActionNode { label = 8, name = "O" }
        , AdActionNode { label = 9, name = "E" }
        , AdActionNode { label = 10, name = "C" }
        , AdActionNode { label = 11, name = "L" }
        , AdActionNode { label = 12, name = "D" }
        , AdObjectNode { label = 13, name = "H" }
        , AdObjectNode { label = 14, name = "N" }
        , AdObjectNode { label = 15, name = "J" }
        , AdObjectNode { label = 16, name = "B" }
        , AdObjectNode { label = 17, name = "F" }
        , AdDecisionNode { label = 18 }
        , AdDecisionNode { label = 19 }
        , AdDecisionNode { label = 20 }
        , AdMergeNode { label = 21 }
        , AdMergeNode { label = 22 }
        , AdMergeNode { label = 23 }
        , AdForkNode { label = 24 }
        , AdJoinNode { label = 25 }
        , AdFlowFinalNode { label = 26 }
        , AdFlowFinalNode { label = 27 }
        , AdInitialNode { label = 28 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 17, guard = "" }
        , AdConnection { from = 2, to = 23, guard = "" }
        , AdConnection { from = 3, to = 27, guard = "" }
        , AdConnection { from = 4, to = 25, guard = "" }
        , AdConnection { from = 5, to = 13, guard = "" }
        , AdConnection { from = 6, to = 11, guard = "" }
        , AdConnection { from = 7, to = 25, guard = "" }
        , AdConnection { from = 8, to = 18, guard = "" }
        , AdConnection { from = 9, to = 19, guard = "" }
        , AdConnection { from = 10, to = 15, guard = "" }
        , AdConnection { from = 11, to = 5, guard = "" }
        , AdConnection { from = 12, to = 21, guard = "" }
        , AdConnection { from = 13, to = 26, guard = "" }
        , AdConnection { from = 14, to = 6, guard = "" }
        , AdConnection { from = 15, to = 23, guard = "" }
        , AdConnection { from = 16, to = 2, guard = "" }
        , AdConnection { from = 17, to = 24, guard = "" }
        , AdConnection { from = 18, to = 14, guard = "b" }
        , AdConnection { from = 18, to = 21, guard = "c" }
        , AdConnection { from = 19, to = 1, guard = "a" }
        , AdConnection { from = 19, to = 22, guard = "b" }
        , AdConnection { from = 20, to = 10, guard = "b" }
        , AdConnection { from = 20, to = 16, guard = "c" }
        , AdConnection { from = 21, to = 8, guard = "" }
        , AdConnection { from = 22, to = 20, guard = "" }
        , AdConnection { from = 23, to = 9, guard = "" }
        , AdConnection { from = 24, to = 3, guard = "" }
        , AdConnection { from = 24, to = 4, guard = "" }
        , AdConnection { from = 24, to = 7, guard = "" }
        , AdConnection { from = 25, to = 12, guard = "" }
        , AdConnection { from = 28, to = 22, guard = "" } 
        ] 
      }
  , drawSettings = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = True }
  , sampleSequence = [ "K", "E", "M", "P", "I", "D", "O", "Q", "L", "A", "G" ]
  , showSolution = True 
  }