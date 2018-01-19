class Test {
  def foo(x: Int) = 1
  val bar: () => Int = foo _ // error: type mismatch
}
