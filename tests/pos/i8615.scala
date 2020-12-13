class ArrayOrdering[N] extends scala.math.Ordering[Array[N]] {
  def compare(x: Array[N], y: Array[N]) = ???
}
class ArrayIntOrdering extends scala.math.Ordering[Array[Int]] {
  def compare(x: Array[Int], y: Array[Int]) = ??? // works fine
}
