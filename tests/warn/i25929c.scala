sealed trait Ev[A, B]
case class Evidence[A]() extends Ev[Some[A], A]

sealed trait Y[A]
case class YInt() extends Y[Int]
case class YString() extends Y[String]

def test[A, B](y: Y[B], ev: Ev[A, B]) =
  ev match
    case Evidence() =>
      y match // warn
        case YInt() => ()
