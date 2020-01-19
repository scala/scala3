import scala.quoted._

object Macros {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('{condition}, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any]) with (qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{_, given _}
    val b = cond.unseal.underlyingArgument.seal.cast[Boolean]
    '{ scala.Predef.assert($b) }
  }

  inline def thisLineNumber = ${ thisLineNumberImpl }

  def thisLineNumberImpl with (qctx: QuoteContext) : Expr[Int] = {
    import qctx.tasty.{_, given _}
    Expr(rootPosition.startLine)
  }
}
