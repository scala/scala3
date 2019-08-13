package lib

class A(val n: Int) {
  val a = 30 * n
  class B(x: Int) {
    val b = x * a
    def bar(i: Int) = i * x
  }

  def foo(i: Int) = i * n
}
