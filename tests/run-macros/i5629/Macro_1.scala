import scala.quoted.*

object Macros {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('{condition}, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    val b = cond.asTerm.underlyingArgument.asExprOf[Boolean]
    '{ scala.Predef.assert($b) }
  }

  inline def thisLineNumber = ${ thisLineNumberImpl }

  def thisLineNumberImpl(using Quotes) : Expr[Int] = {
    import quotes.reflect.*
    Expr(Position.ofMacroExpansion.startLine)
  }
}
