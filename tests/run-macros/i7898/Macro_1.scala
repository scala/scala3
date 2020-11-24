import scala.quoted._

object Main {

  def myMacroImpl(body: Expr[_])(using Quotes) : Expr[_] = {
    import quotes.reflect._
    val bodyTerm = Term.of(underlyingArgument(body))
    val showed = bodyTerm.show
    '{
      println(${Expr(showed)})
      ${bodyTerm.asExpr}
    }
  }

  transparent inline def myMacro(body: => Any): Any = ${
    myMacroImpl('body)
  }

  def underlyingArgument[T](expr: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect._
    Term.of(expr).underlyingArgument.asExpr.asInstanceOf[Expr[T]]
}
