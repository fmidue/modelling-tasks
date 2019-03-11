open PetriNetA_Ordered
open PetriConstraints
open OneLiner

fact{
  no tokenChange
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred showAddOneWeightOnePairConcurrency[]{
  weightAddedOverall[1]
  concurrent[T1 + T3]
}
run showAddOneWeightOnePairConcurrency for 3

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred showRemoveOneWeightOnePairConcurrency[]{
  weightRemovedOverall[1]
  concurrent[T1 + T3]
}
run showRemoveOneWeightOnePairConcurrency for 3

//Add exactly one weight somewhere so that no transitions is activated
pred showAddOneWeightNoActivatedTrans[]{
  weightAddedOverall[1]
  noActivatedTrans
}
run  showAddOneWeightNoActivatedTrans for 3

//Remove exactly one weight somewhere so that no transitions is activated
pred showRemoveOneWeightNoActivatedTrans[]{
  weightRemovedOverall[1]
  noActivatedTrans
}
run  showRemoveOneWeightNoActivatedTrans for 3
