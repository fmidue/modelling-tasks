FindAuxiliaryPetriNodesInstance 
  { activityDiagram = UMLActivityDiagram 
      { nodes = 
        [ AdActionNode { label = 1, name = "D" }
        , AdActionNode { label = 2, name = "F" }
        , AdActionNode { label = 3, name = "I" }
        , AdActionNode { label = 4, name = "A" }
        , AdActionNode { label = 5, name = "C" }
        , AdActionNode { label = 6, name = "J" }
        , AdObjectNode { label = 7, name = "E" }
        , AdObjectNode { label = 8, name = "H" }
        , AdObjectNode { label = 9, name = "G" }
        , AdObjectNode { label = 10, name = "B" }
        , AdDecisionNode { label = 11 }
        , AdDecisionNode { label = 12 }
        , AdMergeNode { label = 13 }
        , AdMergeNode { label = 14 }
        , AdForkNode { label = 15 }
        , AdJoinNode { label = 16 }
        , AdFlowFinalNode { label = 17 }
        , AdFlowFinalNode { label = 18 }
        , AdInitialNode { label = 19 } 
        ]
      , connections = 
        [ AdConnection { from = 1, to = 14, guard = "" }
        , AdConnection { from = 2, to = 16, guard = "" }
        , AdConnection { from = 3, to = 17, guard = "" }
        , AdConnection { from = 4, to = 14, guard = "" }
        , AdConnection { from = 5, to = 18, guard = "" }
        , AdConnection { from = 6, to = 13, guard = "" }
        , AdConnection { from = 7, to = 10, guard = "" }
        , AdConnection { from = 8, to = 3, guard = "" }
        , AdConnection { from = 9, to = 12, guard = "" }
        , AdConnection { from = 10, to = 8, guard = "" }
        , AdConnection { from = 11, to = 6, guard = "b" }
        , AdConnection { from = 11, to = 9, guard = "a" }
        , AdConnection { from = 12, to = 1, guard = "c" }
        , AdConnection { from = 12, to = 4, guard = "b" }
        , AdConnection { from = 13, to = 16, guard = "" }
        , AdConnection { from = 14, to = 13, guard = "" }
        , AdConnection { from = 15, to = 2, guard = "" }
        , AdConnection { from = 15, to = 5, guard = "" }
        , AdConnection { from = 15, to = 11, guard = "" }
        , AdConnection { from = 16, to = 7, guard = "" }
        , AdConnection { from = 19, to = 15, guard = "" } 
        ] 
      }
  , plantUMLConf = PlantUmlConfig 
      { suppressNodeNames = False, suppressBranchConditions = True }
  , showSolution = True 
  }