module PetriAdditions

/*
 * Creates petri nets or reconstructs petri net by adding new places or transitions. For reconstructing petri net,
 * predicate fineGrained will ensure that no change on the given petri net. That is, no token added on given places,
 * no flow added between given places and given transitions. 
 * It is forbidden to reconstruct a ordered petri net, only non-ordered petri net is possible.
*/

open PetriSignature

//Places and Transitions to be added
sig addedPlaces extends Places{}
{
  defaultTokens = 0
  no defaultFlow
}

sig addedTransitions extends Transitions{}
{
  no defaultFlow
}

pred fineGrained[]{
  no givenPlaces.tokenChange
  no givenPlaces.flowChange[givenTransitions]
  no givenTransitions.flowChange[givenPlaces]
}
