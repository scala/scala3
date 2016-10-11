object intersection {

  class A
  class B

  val x: A => Unit = ???
  val y: B => Unit = ???

  val z = if (???) x else y

  val a: A & B => Unit = z
  val b: (A => Unit) | (B => Unit) = z

  type needsA = A => Nothing
  type needsB = B => Nothing
}
