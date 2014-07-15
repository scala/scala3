package test
class B extends A {
  override def foo(x: Int*): Int = x.length + 1
}
object B extends App {
  println(new B().foo(1, 2, 3))
}
