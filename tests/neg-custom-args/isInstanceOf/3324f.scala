trait C[T]
class D[T]

class Test {
  def foo[T](x: C[T]) = x match {
    case _: D[T] =>    // error
    case _: C[Int] =>  // error
  }
}