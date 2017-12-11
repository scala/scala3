import scala.quoted._

object Test {

  def f[T](x: Expr[T])(t0: Type[T]) = {
    implicit val t: Type[T] = t0
    '{
      val y: t.unary_~ = x.unary_~
      val z = ~x
    }
  }

  def f2[T](x: Expr[T])(implicit t: Type[T]) = '{
    val y: t.unary_~ = x.unary_~
    val z = ~x
  }

  f('(2))('[Int])
   f('{ true })('[Boolean])

  def g(es: Expr[String], t: Type[String]) =
    f('{ (~es + "!") :: Nil })('[List[~t]])
}

