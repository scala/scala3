class A[T]
object o {
  def foo(x: Int = 5 6, y Int = 7, z: Int 5, x = 5): Unit = () // error // error // error // error

  case class Plus(a: Int, b: Int)

  object Plus {
    def unapply(r: Int): Plus = Plus(r - 1, 1)
  }
  5 match {
    case Plus(4 1) => // error
    case Plus(4 5 6 7, 1, 2 3) => // error // error
  }
  val x: A[T=Int, T=Int] = ??? // error // error
}
