import scala.quoted._
import scala.tasty._

object Macros {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('{condition}, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    val b = cond.unseal.underlyingArgument.seal.cast[Boolean]
    '{ scala.Predef.assert($b) }
  }

  inline def thisLineNumber = ${ thisLineNumberImpl }

  def thisLineNumberImpl(implicit refl: Reflection): Expr[Int] = {
    import refl._
    refl.rootPosition.startLine.toExpr
  }
}
