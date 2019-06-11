object Test {
  opaque type T[X] = X
  object T {
    def f(x: T[Int]): Int = x // OK
    def g(x: Int): T[Int] = x // OK
  }
}
object Test2 {
  import Test._
  val x: T[Int] = 2 // error
  val y: Int = x    // error
}
