trait Op[T1, T2, +R] {
  def apply(t1: T1, t2: T2): R
}

object Op {
  implicit val compInt: Op[Int, Int, Int] = new Op[Int, Int, Int] {
    def apply(x: Int, y: Int) = scala.math.min(x, y)
  }
}

object Foo {
  def foo(x: Double) = x + 1.0
  def min[T, U, V](x: T, y: U)(implicit op: Op[T, U, V]): V = op(x, y)
  def minInt(x: Int, y: Int)(implicit op: Op[Int, Int, Int]): Int = op(x, y)
  def minR[R](x: Int, y: Int)(implicit op: Op[Int, Int, R]): R = op(x, y)
  min(3, 4) // works in both
  foo(min(3, 4)) // error: works in Scala 2, not in 3
  foo(minInt(3, 4)) // works in both
  foo(minR(3, 4)) // error: works in Scala 2, not in 3
}
