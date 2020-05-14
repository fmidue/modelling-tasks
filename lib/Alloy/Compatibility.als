module Compatibility

open PetriConstraints

//check if maximum set of concurrent transitions
pred isMaxConcurrency[ts : set Transitions]{
  maximallyConcurrent[ts]
}

//altogether exactly n transitions should be activated
pred numberActivatedTransition[n : Int, ts : set Transitions]{
  #ts = n
  theActivatedTransitions[ts]
}

//altogether exactly n tokens should be added
pred tokensAddedOverall[n : Int]{
  tokenAddOnly
  tokenChangeSum[Places] = n
}

//altogether exactly n tokens should be removed
pred tokensRemovedOverall[n : Int]{
  tokenRemoveOnly
  tokenChangeSum[Places] = minus[0,n]
}

//In each place, at most m tokens should be added
pred perPlaceTokensAddedAtMost[m : Int]{
  tokenAddOnly
  all p : Places | p.tokenChange =< m
}

//altogether exactly n weight should be added
pred weightAddedOverall[n : Int]{
  weightAddOnly
  flowChangeSum[Nodes,Nodes] = n
}

//altogether exactly n weight should be removed
pred weightRemovedOverall[n : Int]{
  weightRemoveOnly
  flowChangeSum[Nodes,Nodes] = minus[0,n]
}

//check if there is a loop between a place and a transition
// pred selfLoop[p : Places, t : Transitions]{
//   (one p.flow[t]) and (one t.flow[p])
// }
