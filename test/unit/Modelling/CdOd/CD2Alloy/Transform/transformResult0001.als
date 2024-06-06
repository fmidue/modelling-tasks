
// Alloy Model for CD1
// Produced by Haskell reimplementation of Eclipse plugin transformation
// Generated: -

module cd2alloy/CD1Module

///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified
///////////////////////////////////////////////////

//Names of fields/associations in classes of the model
abstract sig FieldName {}

//Parent of all classes relating fields and values
abstract sig Object {
  get : FieldName -> set Object
}

pred ObjectFieldNames[objects : set Object, fieldNames : set FieldName] {
  no objects.get[FieldName - fieldNames]
}

pred ObjectLowerUpperAttribute[objects : set Object, fieldName : FieldName, fType : set Object, low, up : Int] {
  ObjectLowerAttribute[objects, fieldName, fType, low]
  all o : objects | #o.get[fieldName] =< up
}

pred ObjectLowerAttribute[objects : set Object, fieldName : FieldName, fType : set Object, low : Int] {
  objects.get[fieldName] in fType
  all o : objects | #o.get[fieldName] >= low
}

pred ObjectLowerUpper[objects : set Object, fieldName : FieldName, fType : set Object, low, up : Int] {
  ObjectLower[objects, fieldName, fType, low]
  ObjectUpper[objects, fieldName, fType, up]
}

pred ObjectLower[objects : set Object, fieldName : FieldName, fType : set Object, low : Int] {
  all r : objects | #{l : fType | r in l.get[fieldName]} >= low
}

pred ObjectUpper[objects : set Object, fieldName : FieldName, fType : set Object, up : Int] {
  all r : objects | #{l : fType | r in l.get[fieldName]} =< up
}

pred Composition[left : set Object, lFieldName : set FieldName, right : set Object] {
  // all l1, l2 : left | (#{l1.get[lFieldName] & l2.get[lFieldName]} > 0) => l1 = l2
  all r : right | #{l : left, lF : lFieldName | r in l.get[lF]} =< 1
}


fact LimitIsolatedObjects {
  #Object > mul[2, #{o : Object | no o.get and no get.o}]
}


fact SizeConstraints {
  #Object >= 2
  #get >= 4
  #get <= 10
  all o : Object | let x = plus[#o.get,minus[#get.o,#o.get.o]] | x <= 4

}


fact SomeSelfLoops {
  some o : Object | o in o.get[FieldName]
}


///////////////////////////////////////////////////
// Structures potentially common to multiple CDs
///////////////////////////////////////////////////

// Concrete names of fields


// Classes (non-abstract)
sig A extends Object {}
sig B extends Object {}
sig C extends Object {}
sig D extends Object {}


///////////////////////////////////////////////////
// CD1
///////////////////////////////////////////////////

// Types wrapping subtypes
fun ASubsCD1 : set Object {
  A
}
fun BSubsCD1 : set Object {
  B
}
fun CSubsCD1 : set Object {
  C
}
fun DSubsCD1 : set Object {
  D
}

// Types wrapping field names
fun AFieldNamesCD1 : set FieldName {
  none
}
fun BFieldNamesCD1 : set FieldName {
  none
}
fun CFieldNamesCD1 : set FieldName {
  none
}
fun DFieldNamesCD1 : set FieldName {
  none
}

// Types wrapping composite structures and field names

// Properties

pred cd1 {

  Object = none + A + B + C + D

  // Contents
  ObjectFieldNames[A, AFieldNamesCD1]
  ObjectFieldNames[B, BFieldNamesCD1]
  ObjectFieldNames[C, CFieldNamesCD1]
  ObjectFieldNames[D, DFieldNamesCD1]

  // Associations

  // Compositions

}

