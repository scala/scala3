import scala.quoted._

object Test {
  given QuoteContext = ???

  def f[T](x: Expr[T])(implicit t: TypeTag[T]) = '{
    val y: $t = $x
    val z = $x
  }

  f('{2})('[Int])
  f('{ true })('[Boolean])

  def g(es: Expr[String], t: TypeTag[String]) =
    f('{ ($es + "!") :: Nil })('[List[$t]])
}
