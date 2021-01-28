class A {
  def f(x: Any) = 1

  def foo(x: List[String]): Unit = {
    f(x*) // error
  }
}
