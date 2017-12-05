import scala.meta._

object Test {

  def f[T](t: Type[T], x: Expr[T]) = {
    val y: t.unary_~ = x.unary_~
    val z: ~t = ~x
  }

  f('[Int], '(2))
  f('[Boolean], '{ true })

  def g(es: Expr[String], t: Type[String]) =
    f('[List[~t]], '{ (~es + "!") :: Nil })
}
