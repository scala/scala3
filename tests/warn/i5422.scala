sealed trait Foo[A[_]]

case class Bar[C[_], X](x: C[X]) extends Foo[C]
case class End[B[_]]()           extends Foo[B]

class Test:
  def foo[M[_]](foo: Foo[M]): Int = foo match
    case End()  => 0
    case Bar(_) => 1
