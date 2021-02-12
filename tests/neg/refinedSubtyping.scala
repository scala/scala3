// tests that a refinement subtype satisfies all constraint
// of its refinemen supertype
class Test3 {

  trait A
  trait B

  class C { type T }

  type T1 = C { type T <: A }
  type T2 = T1 { type T <: B }

  type U1 = C { type T <: B }
  type U2 = C { type T <: A }

  var x: T2 = compiletime.uninitialized
  val y1: U1 = ???
  val y2: U2 = ???

  x = y1 // error
  x = y2 // error

}
