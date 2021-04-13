@main def test: Unit = {
  trait S[A]
  trait Inv[A]

  class P[X] extends S[Inv[X] & Inv[String]]

  def patmat[A, Y](s: S[Inv[A] & Y]): A = s match {
    case p: P[x] =>
      "Hello" // error
  }

  val got: Int = patmat[Int, Inv[String]](new P) // ClassCastException: String cannot be cast to Integer
}
