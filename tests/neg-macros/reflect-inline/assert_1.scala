import scala.quoted.*

object api {
  extension (inline x: String) inline def stripMargin2: String =
    ${ stripImpl('x) }

  private def stripImpl(x: Expr[String])(using Quotes): Expr[String] =
    Expr(x.valueOrError.stripMargin)

}
