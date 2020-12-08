import scala.quoted._

object Main {

  def myMacroImpl(body: Expr[_])(using Quotes) : Expr[_] = {
    import quotes.reflect._
    val bodyTerm = underlyingArgument(body).asTerm
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
    expr.asTerm.underlyingArgument.asExpr.asInstanceOf[Expr[T]]
}
