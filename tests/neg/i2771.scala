import language.experimental.namedTypeArguments
trait A { type L[X] }
trait B { type L }
trait C { type M <: A }
trait D { type M >: B }

object Test {
  type LB[F[_]]

  type LL[F[_]] <: LB[F] // ok

  def foo[X[_] <: Any]() = ()
  foo[Int]()  // error: Type argument Int does not conform to upper bound

  def bar[X, Y]() = ()
  bar[List, Int]()   // error: missing type parameter(s) for List

  bar[Y = List, X = Int]()  // error: missing type parameter(s) for List

}
