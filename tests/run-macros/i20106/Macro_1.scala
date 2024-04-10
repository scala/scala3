import scala.quoted.*

object Doc {
  inline def of[A]: Option[String] = ${ ofImpl[A] }

  def ofImpl[A: Type](using Quotes): Expr[Option[String]] = {
    import quotes.reflect.*

    val symbol = TypeRepr.of[A].typeSymbol
    Expr(symbol.docstring)
  }
}
