trait A { type L[X[_]] }
trait B { type L }
trait C { type M <: A }
trait D { type M >: B }

object Test {
  def test(x: C with D): Unit = {
    def f(y: x.M)(z: y.L[y.L]) = z          // error: y.L has wrong kind
    f(new B { type L[F[_[_]]] = F[F] })(1)  // error: F has wrong kind
  }
}
