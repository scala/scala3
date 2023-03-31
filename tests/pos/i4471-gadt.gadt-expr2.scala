sealed trait Foo[A, B]
case class Id[T]() extends Foo[T, T]

// A minimisation of pos/i4471-gadt to fix its breakage while implementing GadtExpr
class Test:
  def mat[X, Y](scr: Foo[X, Y]) = scr match
    case Id() =>
