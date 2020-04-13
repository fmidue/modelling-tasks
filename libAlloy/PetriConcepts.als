//check if a transition is activated
pred activated[t : Transitions]{
  all p : Places | p.tokens >= p.flow[t]
}

//check if a transition conflicts with another transitions
pred conflict[t1, t2 : Transitions, p : Places]{
  t1 != t2
  activated[t1]
  activated[t2]
  p.tokens < plus[p.flow[t1], p.flow[t2]]
}

//check if two distinct transitions are concurrent
pred concurrent[ts : set Transitions]{
  all p : Places | p.tokens >= flowSum[p, ts]
}

//check activation under default condition
 pred activatedDefault[t : Transitions]{
  all p : Places | p.defaultTokens >= p.defaultFlow[t]
}

//check conflict under default condition
pred conflictDefault[t1, t2 : Transitions, p : Places]{
  t1 != t2
  activatedDefault[t1]
  activatedDefault[t2]
  p.defaultTokens < plus[p.defaultFlow[t1], p.defaultFlow[t2]]
}

//check concurrent under default condition
pred concurrentDefault[ts : set Transitions]{
  all p : Places | p.defaultTokens >= defaultFlowSum[p, ts]
}

//check if there is a loop between a place and a transition
pred selfLoop[p : Places, t : Transitions]{
  (one p.flow[t]) and (one t.flow[p])
}

//check if some transitions are sink transitions
pred sinkTransitions[ts : set Transitions]{
  no ts.flow
}

//check if some transitions are source transitions
pred sourceTransitions[ts : set Transitions]{
  no Places.flow[ts]
}
