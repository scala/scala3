class A(x: Int)(y: String = "hi")

object Test {
  def f() = 22

  class B extends A(f())()

}