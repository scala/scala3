trait A { type L[X] }
trait B { type L }
trait C { type M <: A }
trait D { type M >: B }

object Test {
  def test(x: C with D): Unit = {
    def f(y: x.M)(z: y.L[y.L]) = z      // error: y.L takes type parameters
    f(new B { type L[F[_]] = F[F] })(1) // error: F takes type parameters
  }

  def foo[X[_] <: Any]() = ()
  foo[Int]()  // should be an error

  def bar[X, Y]() = ()
  bar[List, Int]()   // error: List takes type parameters

  bar[Y = List, X = Int]  // error: List takes type parameters

}
