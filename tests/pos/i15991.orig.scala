class Foo

object Foo:
//  def unapply(f: Foo): (Int, Int) = ???  // does not raise a warning
  def unapply(f: Foo): Int *: Int *: EmptyTuple = ???

@main def example =
  val Foo(x, y) = new Foo
  println(x)
