class A[S, T](s: S, t: T) {
  val z: T = ???
}

class B[X](x: X) extends A[X, X](x, x) {
  def this() = this(z) // error: not found: z

  val u: X = x
  def this(x: Int) = this(u) // error: not found: u
}

