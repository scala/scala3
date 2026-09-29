import scala.quoted.*

object M:
  transparent inline def apply(inline expr: Any): Any =
    ${ impl('expr) }

  private def impl(e: Expr[Any])(using Quotes): Expr[Any] = e
