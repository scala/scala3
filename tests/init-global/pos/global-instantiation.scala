class A(x: Int) {
  def foo(): Int = B.m
}

object B:
  val m: Int = 20
  val n: Int = new A(10).foo()