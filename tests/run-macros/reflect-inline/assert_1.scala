import scala.quoted._

object api {
  extension (inline x: String) inline def stripMargin: String =
    ${ stripImpl('x) }

  private def stripImpl(x: Expr[String])(using Quotes): Expr[String] =
    Expr(augmentString(x.unliftOrError).stripMargin)

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl('{scala.compiletime.testing.typeChecks(x)}) }

  private def typeChecksImpl(b: Expr[Boolean])(using Quotes): Expr[Boolean] = {
    if (b.unliftOrError) Expr(true) else Expr(false)
  }
}
