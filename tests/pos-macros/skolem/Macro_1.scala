import scala.quoted.*

object Macro {

  def impl(expr: Expr[Any])(using Quotes): Expr[Unit] =
    val s = expr.show
    '{ () }

  inline def macr(inline x: Any): Unit = ${impl('x)}
}
