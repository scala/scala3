trait A { type L[G[F[_],_],H[F[_],_]] }
trait B { type L[F[_],_] }
trait C { type M <: A }
trait D { type M >: B }

object Test {
  def test(x: C with D): Unit = {
    def foo(a: A, b: B)(z: a.L[b.L,b.L]) = z
    def bar(y: x.M, b: B) = foo(y, b)
    def baz(b: B) = bar(b, b)
    baz(new B { type L[F[_],X] = F[X] })(1) // error
  }
}
