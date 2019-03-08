opaque type T[X] = X
object T {
  def f(x: T[Int]): Int = x // OK
  def g(x: Int): T[Int] = x // OK
}
