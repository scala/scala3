class C { type T }

class Test {

  type D <: C

  lazy val a: C = ???
  final lazy val b: C = ???
  val c: D = ???
  final lazy val d: D = ???

  val x1: a.T = ???  // error: not a legal path, since a is lazy & non-final
  val x2: b.T = ???  // OK, b is lazy but concrete
  val x3: c.T = ???  // OK, c is abstract but strict
  val x4: d.T = ???  // error: not a legal path since d is abstract and lazy

  val y1: Singleton = a
  val y2: Singleton = a
  val y3: Singleton = a
  val y4: Singleton = a

}
