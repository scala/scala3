class C[T]

object Test {
  def f(x: C[Int]) = ???

  f(new C {})
}
