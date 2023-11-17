abstract class A(x: Int) {
  def foo(): Unit
  foo()
}

trait B(val y: Int)    // warn

class C extends A(10) with B(20)  {
  def foo(): Unit = println(y)
}