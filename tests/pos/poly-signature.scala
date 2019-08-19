trait P[A] {
  def foo[T](x: Int): A = ???
}

class C extends P[Int] {
  def foo(x: Int): Int = x
}

object Test {
  def test(p: C): Unit = {
    p.foo(1)
  }
}
