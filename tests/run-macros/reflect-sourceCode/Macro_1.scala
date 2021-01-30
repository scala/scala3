import scala.quoted.*

object api {
  extension [T](x: => T) inline def reflect: String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit qctx: Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(x.asTerm.pos.sourceCode.get)
  }
}
