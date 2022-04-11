trait A { type S[X[_] <: [_] =>> Any, Y[_]] <: [_] =>> Any; type I[_] }
trait B { type S[X[_],Y[_]]; type I[_] <: [_] =>> Any }
trait C { type M <: B }
trait D { type M >: A }

object Test {
  def test(x: C with D): Unit = {
    def foo(a: A, b: B)(z: a.S[b.I,a.I][b.S[a.I,a.I]]) = z
    def bar(a: A, y: x.M) = foo(a,y)
    def baz(a: A) = bar(a, a)
    baz(new A { type S[X[_] <: [_] =>> Any, Y[_]] = [Z] =>> X[Z][Y[Z]]; type I[X] = X })(1) // error
  }
}