// Taken from
// https://github.com/lampepfl/dotty/issues/11545#issuecomment-787609144
class test0 {
  type AA
  trait S[A]
  trait Inv[A]

  class P[X] extends S[Inv[X] & AA]

}

@main def test: Unit =
  new test0:
    type AA = Inv[String]

    def patmat[A, Y](s: S[Inv[A] & Y]): A = s match {
      case p: P[x] =>
        "Hello"  // error
    }

    val got: Int = patmat[Int, AA](new P)
