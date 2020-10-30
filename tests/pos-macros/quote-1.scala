import scala.quoted._

class Test(using QuoteContext) {

  def f[T](x: Expr[T])(implicit t: Type[T]) = '{
    val y: T = $x
    val z = $x
  }

  f('{2})(Type[Int])
  f('{ true })(Type[Boolean])

  def g(es: Expr[String], t: Type[String]) =
    f('{ ($es + "!") :: Nil })(Type[List[t.Underlying]])
}
