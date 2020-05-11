import scala.quoted._

class Test(using val s: Scope) {

  def f[T](x: s.Expr[T])(implicit t: s.Type[T]) = '{
    val y: $t = $x
    val z = $x
  }

  f('{2})('[Int])
  f('{ true })('[Boolean])

  def g(es: s.Expr[String], t: s.Type[String]) =
    f('{ ($es + "!") :: Nil })('[List[$t]])
}
