sealed trait Foo[A]

case class Foo1[X]() extends Foo[(X, Int)]
case class Foo2[Y]() extends Foo[(Y, Int)]

// A minimisation of pos/i4471-gadt to fix its breakage while implementing GadtExpr
class Test:
  def test[T](f1: Foo[T], f2: Foo[T]) = (f1, f2) match
    case (_: Foo1[x], _: Foo2[y]) => 1
    case _ => 2
