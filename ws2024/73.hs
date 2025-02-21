FindAuxiliaryPetriNodesInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "C" }
        , AdActionNode { label = 2, name = "B" }
        , AdActionNode { label = 3, name = "A" }
        , AdActionNode { label = 4, name = "I" }
        , AdActionNode { label = 5, name = "F" }
        , AdActionNode { label = 6, name = "G" }
        , AdActionNode { label = 7, name = "L" }
        , AdActionNode { label = 8, name = "K" }
        , AdObjectNode { label = 9, name = "D" }
        , AdObjectNode { label = 10, name = "H" }
        , AdObjectNode { label = 11, name = "E" }
        , AdObjectNode { label = 12, name = "J" }
        , AdDecisionNode { label = 13 }
        , AdDecisionNode { label = 14 }
        , AdDecisionNode { label = 15 }
        , AdMergeNode { label = 16 }
        , AdMergeNode { label = 17 }
        , AdMergeNode { label = 18 }
        , AdForkNode { label = 19 }
        , AdJoinNode { label = 20 }
        , AdActivityFinalNode { label = 21 }
        , AdInitialNode { label = 22 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 20, guard = "" }
        , AdConnection { from = 2, to = 20, guard = "" }
        , AdConnection { from = 3, to = 16, guard = "" }
        , AdConnection { from = 4, to = 11, guard = "" }
        , AdConnection { from = 5, to = 6, guard = "" }
        , AdConnection { from = 6, to = 17, guard = "" }
        , AdConnection { from = 7, to = 16, guard = "" }
        , AdConnection { from = 8, to = 19, guard = "" }
        , AdConnection { from = 9, to = 4, guard = "" }
        , AdConnection { from = 10, to = 20, guard = "" }
        , AdConnection { from = 11, to = 21, guard = "" }
        , AdConnection { from = 12, to = 1, guard = "" }
        , AdConnection { from = 13, to = 9, guard = "b" }
        , AdConnection { from = 13, to = 17, guard = "a" }
        , AdConnection { from = 14, to = 2, guard = "b" }
        , AdConnection { from = 14, to = 18, guard = "c" }
        , AdConnection { from = 15, to = 3, guard = "c" }
        , AdConnection { from = 15, to = 7, guard = "b" }
        , AdConnection { from = 16, to = 14, guard = "" }
        , AdConnection { from = 17, to = 8, guard = "" }
        , AdConnection { from = 18, to = 15, guard = "" }
        , AdConnection { from = 19, to = 10, guard = "" }
        , AdConnection { from = 19, to = 12, guard = "" }
        , AdConnection { from = 19, to = 18, guard = "" }
        , AdConnection { from = 20, to = 13, guard = "" }
        , AdConnection { from = 22, to = 5, guard = "" } 
        ] 
      }
  , plantUMLConf = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = True }
  , showSolution = True 
  }