import scala.quoted._

object api {
  extension (inline x: String) inline def stripMargin: String =
    ${ stripImpl('x) }

  private def stripImpl(using s: Scope)(x: s.Expr[String]): s.Expr[String] =
    Expr(augmentString(x.unliftOrError).stripMargin)

  inline def typeChecks(inline x: String): Boolean =
    ${ typeChecksImpl('{scala.compiletime.testing.typeChecks(x)}) }

  private def typeChecksImpl(using s: Scope)(b: s.Expr[Boolean]): s.Expr[Boolean] = {
    if (b.unliftOrError) Expr(true) else Expr(false)
  }
}
