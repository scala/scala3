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
  def g: C[A | B] = ???
  def h: C[A] & C[B] = g
}
object Test {

  trait A {
    def f: Any
  }
  trait B extends A {
    override def f: Int = 1
  }
  trait C extends A {
    def f: Any = ""
  }

  val bc: B with C = new C with B {}

  def fooAB = (??? : A with B).f
  def fooAB1: Int = fooAB
  def fooBA = (??? : B with A).f
  def fooBA1: Int = fooBA
}
