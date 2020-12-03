import scala.quoted._

object api {
  extension [T](x: => T) inline def reflect: String =
    ${ reflImpl('x) }

  private def reflImpl[T](x: Expr[T])(implicit qctx: Quotes): Expr[String] = {
    import quotes.reflect._
    Expr(Term.of(x).pos.sourceCode.get)
  }
}
