@main def test: Unit = {
  trait S[A]
  trait Inv[A]

  locally {
    class P[X] extends S[Inv[X] & Inv[String]]   // error: cannot be instantiated

    def patmat[A, Y](s: S[Inv[A] & Y]): A = s match {
      case p: P[x] =>
        "Hello"
    }

    val got: Int = patmat[Int, Inv[String]](new P)
  }

  locally {
    class P[X] extends S[S[Inv[X] & Inv[String]]]  // error: cannot be instantiated

    def patmat[A, Y](s: S[S[Inv[A] & Y]]): A = s match {
      case p: P[x] =>
        "Hello"
    }

    val got: Int = patmat[Int, Inv[String]](new P)
  }

  locally {
    abstract class P[X] extends S[Inv[X] & Inv[String]]

    def patmat[A, Y](s: S[Inv[A] & Y]): A = s match {
      case p: P[x] =>
        "Hello"
    }

    val got: Int = patmat[Int, Inv[String]](new P {}) // error: cannot be instantiated
  }
}