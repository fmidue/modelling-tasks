module PetriAdditions

open PetriSignature

//Place and Transition to be added
sig addedPlace extends Place{}
{
  defaultTokens in 0
  no defaultFlow
}
sig addedTransition extends Transition{}
{
  no defaultFlow
}
