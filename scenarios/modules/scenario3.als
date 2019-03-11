open PetriNetA_Ordered
open PetriConstraints
open OneLiner

fact{
  no flowChange
}

//Remove a token so that there are no activated transitions.
pred showAddOneTokenOnePairConcurrency[]{
  tokensAddedOverall[1]
  concurrent[T1 + T3]
}
run showAddOneTokenOnePairConcurrency for 3

//Remove a token so that there are no activated transitions.
pred showRemoveOneTokenNoActivatedTransition[]{
  tokensRemovedOverall[1]
  noActivatedTrans
}
run showRemoveOneTokenNoActivatedTransition for 3

//Remove a token so that 2 previously concurrently activated transitions get into conflict
pred showRemoveOneTokenIntoConflict[t1, t2 : Transition]{
  concurrentDefault[t1 + t2]
  tokensRemovedOverall[1]
  gt[t2,t1]
  some placeConflict : Place | conflict[t1, t2, placeConflict]
}
run showRemoveOneTokenIntoConflict

pred showActivatedDefault[t : Transition]{
  no tokenChange
  activatedDefault[t]
}
run showActivatedDefault for 3

pred showConflictDefault[t1, t2 : Transition]{
  no tokenChange
  gt[t2,t1]
  some placeConflictDefault : Place | conflictDefault[t1, t2, placeConflictDefault]
}
run showConflictDefault for 3
