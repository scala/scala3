import scala.quoted._

object Macros {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('{condition}, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    val b = Term.of(cond).underlyingArgument.asExprOf[Boolean]
    '{ scala.Predef.assert($b) }
  }

  inline def thisLineNumber = ${ thisLineNumberImpl }

  def thisLineNumberImpl(using qctx: QuoteContext) : Expr[Int] = {
    import qctx.reflect._
    Expr(Position.ofMacroExpansion.startLine)
  }
}
