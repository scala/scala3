object `implicit-match-nested` {
  import compiletime._

  case class A[T]()
  case class B[T]()

  implicit val a: A[Int] = A[Int]()
  implicit val b1: B[Int] = B[Int]()
  implicit val b2: B[String] = B[String]()

  transparent inline def locateB: B[_] =
    inline summonInlineOpt[A[_]] match {
      case Some(_: A[t]) => summonInline[B[t]]
    }

  locateB
}
