abstract sig Place
{
  inp : Transition -> lone Int,
  defaultInp : Transition -> lone Int,
  inpChange : Transition -> lone Int,
  tokens : one Int
}
{
  tokens >= 0
  all t : Transition | inp[t] = plus[defaultInp[t], inpChange[t]]
}

abstract sig Transition
{
  out : Place -> lone Int,
  defaultOut : Place -> lone Int,
  outChange : Place -> lone Int
}
{
  all p : Place | out[p] = plus[defaultOut[p], outChange[p]]
}

fact {
  all weight : Place.inp[Transition] | weight >= 0
  all weight : Transition.out[Place] | weight >= 0
}

pred activated[t : Transition]{
  all p : Place | p.tokens >= p.inp[t]
}

pred conflict[t1, t2 : Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  some p : Place | p.tokens < plus[p.inp[t1], p.inp[t2]]
}

pred concurrency[t1, t2 : Transition]{
  t1 != t2
  activated[t1]
  activated[t2]
  not conflict[t1,t2]
}

//Add exactly one weight somewhere so that two transitions are concurrently activated
pred addOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all p : Place, t : Transition | p.inpChange[t] >= 0 and t.outChange[p] >=0
  plus[#(Place.inpChange), #(Transition.outChange)] = 1
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] = 1
  concurrency[t1,t2]
}

//Remove exactly one weight somewhere so that two transitions are concurrently activated
pred removeOneWeightOnePairConcurrency[t1, t2 : Transition]{
  all p : Place, t : Transition | p.inpChange[t] =< 0 and t.outChange[p] =< 0
  plus[#(Place.inpChange), #(Transition.outChange)] = 1
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] = (-1)
  concurrency[t1,t2]
}

//Add exactly one weight somewhere so that no transitions is activated
pred addOneWeightNoActivatedTrans[]{
  all p : Place, t : Transition | p.inpChange[t] >= 0 and t.outChange[p] >= 0
  plus[#(Place.inpChange), #(Transition.outChange)] = 1
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] = 1
  all t : Transition | not activated[t]
}

//Remove exactly one weight somewhere so that no transitions is activated
pred removeOneWeightNoActivatedTrans[]{
  all p : Place, t : Transition | p.inpChange[t] =< 0 and t.outChange[p] =< 0
  plus[#(Place.inpChange), #(Transition.outChange)] = 1
  plus[(sum p : Place, t : Transition | p.inpChange[t]), (sum t : Transition, p : Place | t.outChange[p])] = (-1)
  all t : Transition | not activated[t]
}

//default Petri net

one sig S1 extends Place{}
one sig S2 extends Place{}
one sig S3 extends Place{}
one sig T1 extends Transition{}
one sig T2 extends Transition{}
one sig T3 extends Transition{}

fact {
  S1.tokens = 1
  S2.tokens = 1
  S3.tokens = 0

  S1.defaultInp[T1] = 1
  S1.defaultInp[T2] = 1
  S1.defaultInp[T3] = 1

  S2.defaultInp[T2] = 1
  no S2.defaultInp[Transition - T2]

  S3.defaultInp[T2] = 1
  no S3.defaultInp[Transition - T2]

  T1.defaultOut[S2] = 1
  no T1.defaultOut[Place - S2]

  no T2.defaultOut[Place]

  T3.defaultOut[S3] = 1
  no T3.defaultOut[Place - S3]
}

pred showAddOneWeightOnePairConcurrency[t1, t2 : Transition]{
  t1 = T1
  t2 = T3
 addOneWeightOnePairConcurrency[t1,t2]
}
run showAddOneWeightOnePairConcurrency for 3

pred showRemoveOneWeightOnePairConcurrency[t1, t2 : Transition]{
  t1 = T1
  t2 = T3
 removeOneWeightOnePairConcurrency[t1,t2]
}
run showRemoveOneWeightOnePairConcurrency for 3

pred showAddOneWeightNoActivatedTrans[]{
  addOneWeightNoActivatedTrans
}
run  showAddOneWeightNoActivatedTrans for 3

pred showRemoveOneWeightNoActivatedTrans[]{
  removeOneWeightNoActivatedTrans
}
run  showRemoveOneWeightNoActivatedTrans for 3
