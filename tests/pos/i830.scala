object C {
  trait X[T]
  implicit def u[A, B]: X[A | B] = new X[A | B] {}
  def y[T](implicit x: X[T]): T = ???
  val x: 1 & 2 | 2 & 3 = y
}
