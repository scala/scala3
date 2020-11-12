import scala.quoted._

object Main {

  def myMacroImpl(body: Expr[_])(using qctx: QuoteContext) : Expr[_] = {
    import qctx.reflect._
    val bodyTerm = underlyingArgument(body).unseal
    val showed = bodyTerm.show
    '{
      println(${Expr(showed)})
      ${bodyTerm.asExpr}
    }
  }

  transparent inline def myMacro(body: => Any): Any = ${
    myMacroImpl('body)
  }

  def underlyingArgument[T](expr: Expr[T])(using qctx: QuoteContext): Expr[T] =
    expr.unseal.underlyingArgument.asExpr.asInstanceOf[Expr[T]]
}
