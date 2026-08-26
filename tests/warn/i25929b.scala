sealed trait Ev[A]
case class IsInt() extends Ev[Int]

sealed trait X[A]
case class IntX() extends X[Int]
case class StringX() extends X[String]

def test[A](x: X[A], ev: Ev[A]) =
  ev match
    case IsInt() =>
      x match
        case IntX() => ()
