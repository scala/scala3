object intersection {

  class A
  class B

  val x: A => Unit = ???
  val y: B => Unit = ???

  val z = if (???) x else y

  val a: A & B => Unit = z
  //val b: (A => Unit) | (B => Unit) = z // error under new or-type rules

  val c: (A => Unit) | (B => Unit) = if (???) x else y // ok

  type needsA = A => Nothing
  type needsB = B => Nothing


  class C[-T]
  def f: C[A] & C[B] = ???
  def g: C[A | B] = f
  def h: C[A] & C[B] = g
}
