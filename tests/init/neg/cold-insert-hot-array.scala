object A:
  def foo[T](x: T, array: Array[T]): Unit = array(0) = x

class B {
  var a = new Array[B](2)
  A.foo(this, a) // error
  println(a(0).i)
  val i = 99
}
