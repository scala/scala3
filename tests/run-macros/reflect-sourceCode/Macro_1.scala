import scala.quoted._

object api {
  extension [T](x: => T) inline def reflect: String =
    ${ reflImpl('x) }

  private def reflImpl[T](using s: Scope)(x: s.Expr[T]): s.Expr[String] =
    Expr(x.pos.sourceCode)

}
