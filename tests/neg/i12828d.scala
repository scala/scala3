trait A[X] {
  def foo(x: X): Unit =
    println("A.foo")
}
trait B[X] extends A[X] {
  def foo(x: Int): Unit =
    println("B.foo")
}
object C extends B[Int] // error: conflicting members
                        // Scala 2: same

object Test {
  def main(args: Array[String]) = {
    C.foo(1)
    val a: A[Int] = C
    a.foo(1)
  }
}