module exerciseRules

open plantUml as components

//Keep activity finals out of parallel sections in order to avoid confusion of students
pred noActivityFinalInForkBlocks {
        no ae1 : ActivityFinalNodes | ae1 in nodesInThisAndDeeper[PlantUMLForkBlocks]
}

pred noDirectFinalAfterFork {
  no f1 : FinalNodes | f1 in (from.ForkNodes.to)
}

fact {
  noDirectFinalAfterFork
}
