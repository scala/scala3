sealed trait Ev[F[_]]
case class IsList() extends Ev[List]

sealed trait X[A]
case class ListIntX() extends X[List[Int]]
case class AnotherListIntX() extends X[List[Int]]
case class OptionIntX() extends X[Option[Int]]

def test[F[_]](x: X[F[Int]], ev: Ev[F]) =
  ev match
    case IsList() =>
      x match // warn
        case ListIntX() => ()
