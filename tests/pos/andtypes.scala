class Outer {
  type Foo[X]
}

object Test {
  def foo[T](x: T) = x

  def bla[A <: Outer, B <: Outer](a: A, b: B) = {
    val x: a.Foo[Int] & b.Foo[Int] = ???
    foo(x)
  }
}
