object A {
  def foo[T](x: T, y: T): Int = 55
  def bar[T](x: T, y: T): List[T] = x :: y :: Nil
  val x = foo(23, 23f)
  val y = bar(23, 23f)
  val z = List(23, 23f)
  val x2 = foo(23.0, 23)
  val y2 = bar(23.0, 23)
  val z2 = List(23.0, 23)
}