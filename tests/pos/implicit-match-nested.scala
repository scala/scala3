object `implicit-match-nested` {
  case class A[T]()
  case class B[T]()

  implicit val a: A[Int] = A[Int]()
  implicit val b1: B[Int] = B[Int]()
  implicit val b2: B[String] = B[String]()

  inline def locateB <: B[_] = implied match {
    case _: A[t] => implied match {
      case b: B[`t`] => b
    }
  }

  locateB
}
