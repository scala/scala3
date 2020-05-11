import scala.quoted._

object api {
  extension (inline x: String) inline def stripMargin2: String =
    ${ stripImpl('x) }

  private def stripImpl(using s: Scope)(x: s.Expr[String]): s.Expr[String] =
    Expr(x.unliftOrError.stripMargin)

}
