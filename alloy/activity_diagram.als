module project/alloy/activity_diagram // It is used to generate state diagrams that can be converted to activity diagrams

open project/sd_generate/alloy/sd/uml_state_diagram as uml_state_diagram

//Represents an action or object node
abstract sig ActionObjectNodes extends NormalStates {} 
{
	one name //All action or object nodes should have names
	//EmptyTrigger =  (Flows <: from) . this  . label //Dont need named edges
}

//Represents an action node
abstract sig ActionNodes extends ActionObjectNodes {}

//Represents an object node
abstract sig ObjectNodes extends ActionObjectNodes {}


//Represents a decision node
abstract sig DecisionNodes extends NormalStates {}
{
	no name //Decision Nodes dont have names
	(one (Flows <: to) . this) and (not lone (Flows <: from) . this . to)  //Exactly one incoming and more than one outgoing edge
	EmptyTrigger not in (Flows <: from) .this. label //Decisions should have names
	not this in (Flows <: from) .this . to //No reflexive edges, might be strict but seems kind of nonsensical otherwise
}

//Represents a merge node
abstract sig MergeNodes extends NormalStates {}
{
	no name //Merge Nodes dont have names
	(one (Flows <: from) . this) and (not lone (Flows <: to) . this . from) //Exactly one outgoing and more than one incoming edge
	//EmptyTrigger =  (Flows <: from) . this  . label //Dont need named edges
	not this in (Flows <: from) .this . to //No reflexive edges, might be strict but seems kind of nonsensical otherwise
}


//Restrict Nodes to only those usable in Activity Diagrams
pred restrictAllowedNodeTypes {
	Nodes in (StartNodes + EndNodes + ActionObjectNodes + DecisionNodes + MergeNodes  + RegionsStates + ForkNodes + JoinNodes)
}

// Action or Object Nodes should have distinct names
pred actionObjectNodesHaveDistinctNames {
	no disjoint s1,s2 : ActionObjectNodes | s1.name = s2.name
}

//To get "disjunct" decisions for our diagrams
pred edgesFromDecisionNodesHaveDistinctLabels {
	all d1: DecisionNodes | no disjoint f1, f2 : (Flows <: from) . d1 |
		f1.label = f2.label 
}

//A merge node should be reachable by at least one flow which originated from a decision node 
pred flowsToMergeNodeOriginateFromDecisionNode {
	all m1: MergeNodes | 
		m1 in DecisionNodes . ^(~from.to) 
}

//Regions are mapped to parallel flows and dont need names
pred noRegionNames {
	no Regions.name
}

//Prohibit nested regions (maybe too strict?) (maybe bound by Integer)
pred regionsAreFlat {
	all r1:Regions |
		no regionsInThisAndDeeper[r1]
}

//Prevent exits from regions except via Join Nodes
pred permitExitOnlyViaJoin {
	let inner = RegionsStates + Regions.contains |
		no ((Flows <: from).inner.to & ((Nodes - inner) - JoinNodes))
}

//Prevent entries to regions except via Fork Nodes
pred permitEntryOnlyViaFork {
	let inner = RegionsStates + Regions.contains |
		no ((Flows <: to).inner.from & ((Nodes - inner) - ForkNodes))
}

//TODO: Predicates for explicitly setting the number of occurence for each component

pred restrictNumberOfDecisionOrMergeNodes {
	mul[2, #(DecisionNodes + MergeNodes)] <= #ActionObjectNodes
}

pred scenario{
	restrictAllowedNodeTypes
	actionObjectNodesHaveDistinctNames
	//edgesFromDecisionNodesHaveDistinctLabels //already established in transition_rules.als
	//flowsToMergeNodeOriginateFromDecisionNode //Not completely necessary and a bit slow
	noRegionNames
	regionsAreFlat
	permitExitOnlyViaJoin
	permitEntryOnlyViaFork
	restrictNumberOfDecisionOrMergeNodes
	some (DecisionNodes + MergeNodes)
	#Regions = 2
	one EndNodes //Not necessary
	one StartNodes //Not necessary
	EndNodes not in allContainedNodes //Not necessary
	StartNodes not in allContainedNodes //Not necessary
	all s : NormalStates | some (Flows <: from).s
	//EmptyTrigger not in from . States . label 
	some (ForkNodes + JoinNodes)
	JoinNodes not in allContainedNodes
	ForkNodes not in allContainedNodes
	let inner = Regions.contains |
		some ((Flows <: from).inner.to & (JoinNodes - inner)) 
	let inner = Regions.contains |
		some ((Flows <: to).inner.from & (ForkNodes - inner))
	mul[2,#Regions.contains] >= #Nodes
	#Nodes >= 8
}

run scenario for  15 but 6 Int, exactly 1 StartNodes, exactly 1 EndNodes,  exactly 2 Regions, 0 HierarchicalStates, exactly 1 RegionsStates, exactly 1 ForkNodes, exactly 1 JoinNodes, 0 HistoryNodes
