import scala.quoted._
import scala.quoted.show.SyntaxHighlight.ANSI
import scala.tasty._

object api {
  inline def (x: => String) reflect : String =
    ${ reflImpl('x) }

  private def reflImpl(x: Expr[String]) given (qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    x.show.toExpr
  }

  inline def (x: => String) reflectColor : String =
    ${ reflImplColor('x) }

  private def reflImplColor(x: Expr[String]) given (qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    x.show(ANSI).toExpr
  }
}
