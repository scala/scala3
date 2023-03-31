sealed trait Foo[S, T]

case class Bar[A, B](_1: Foo[A, A]) extends Foo[(A, A), (B, B)]
case class Baz[F, G]()              extends Foo[(F, G), (G, F)]

// A minimisation of pos/i4471-gadt to fix its breakage while implementing GadtExpr
class Test:
  def meth[X, Y, Z](x: Foo[X, Y], y: Foo[Y, Z]) = (x, y) match
    case (z: Bar[a, b], _: Bar[c, d]) => (z._1 : Foo[a, a]) match
      case _: Baz[f, g] =>
