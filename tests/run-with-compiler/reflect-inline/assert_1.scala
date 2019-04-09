import scala.quoted._
import scala.tasty._

object api {
  inline def (inline x: String) stripMargin: String =
    ${ stripImpl(x) }

  private def stripImpl(x: String)(implicit refl: Reflection): Expr[String] =
    x.stripMargin.toExpr

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl(x) }

  private def typeChecksImpl(x: String)(implicit refl: Reflection): Expr[Boolean] = {
    import refl._
    if (refl.typing.typeChecks(x)) true.toExpr else false.toExpr
  }
}
