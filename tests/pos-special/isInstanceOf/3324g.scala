class Test {
  trait A[+T]
  class B[T] extends A[T]

  def bar[T](b: B[T]): Unit = b match {
    case _: A[T] =>
  }
}
