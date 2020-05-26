object `implicit-match-nested` {
  import compiletime.summonFrom

  case class A[T]()
  case class B[T]()

  implicit val a: A[Int] = A[Int]()
  implicit val b1: B[Int] = B[Int]()
  implicit val b2: B[String] = B[String]()

  transparent inline def locateB: B[_] =
    summonFrom {
      case _: A[t] =>
        summonFrom {
          case b: B[`t`] => b
        }
    }

  locateB
}
