trait A { type L[X] }
trait B { type L }
trait C { type M <: A }
trait D { type M >: B }

object Test {
  def test(x: C with D): Unit = {
    def f(y: x.M)(z: y.L[y.L]) = z      // error: y.L has wrong kind
    f(new B { type L[F[_]] = F[F] })(1) // error: F has wrong kind
  }

  type LB[F[_]]

  type LL[F[_]] <: LB[F] // ok

  def foo[X[_] <: Any]() = ()
  foo[Int]()  // an error would be raised later, during PostTyper.

  def bar[X, Y]() = ()
  bar[List, Int]()   // error: List has wrong kind

  bar[Y = List, X = Int]()  // error: List has wrong kind

}
