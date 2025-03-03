import scala.quoted.*

object Macro:
  inline def showType(inline expr: Any): String = ${showType('expr)}

  private def showType(expr: Expr[Any])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(expr.asTerm.tpe.widen.show)
