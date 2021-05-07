import scala.quoted.*

inline def getDocString[T]: Option[String] = ${ getDocStringImpl[T] }

private def getDocStringImpl[T : Type](using Quotes): Expr[Option[String]] = {
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.docstring)
}
