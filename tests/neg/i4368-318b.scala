object i318 {
  trait Y {
    type A <: { type T >: B }
    type B >: { type T >: A }
  }

  val y: Y = ???
  val b: y.B = ???   // error: too deep
}
