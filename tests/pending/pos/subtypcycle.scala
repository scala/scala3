object subtypcycle {
  trait Y {
    type A <: { type T >: B }
    type B >: { type T >: A }
  }

  val y: Y = ???
  val a: y.A = ???
  val b: y.B = a
}
