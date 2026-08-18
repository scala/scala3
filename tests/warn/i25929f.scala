sealed trait Ev[F[_], G[_]]
case class Same[H[_]]() extends Ev[H, H]

sealed trait X[A]
case class XL() extends X[List[Int]]
case class XO() extends X[Option[Int]]

def test[F[_], G[_]](x: X[F[Int]], ev: Ev[F, G]) =
  ev match
    case Same() =>
      x match // warn
        case XL() => ()
