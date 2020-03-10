import scala.quoted._

class Test(using QuoteContext) {

  def f[T](x: Expr[T])(implicit t: Type[T]) = '{
    val y: $t = $x
    val z = $x
  }

  f('{2})('[Int])
  f('{ true })('[Boolean])

  def g(es: Expr[String], t: Type[String]) =
    f('{ ($es + "!") :: Nil })('[List[$t]])
}
