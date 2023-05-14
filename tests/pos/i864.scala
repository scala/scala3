object C {
  val a: Int = 1
  val b: Int = 2
  val c: Int = 2

  trait X[T]
  implicit def u[A, B]: X[A | B] = new X[A | B] {}
  def y[T](implicit x: X[T]): T = ???
  val x: a.type & b.type | b.type & c.type = y // error
}
