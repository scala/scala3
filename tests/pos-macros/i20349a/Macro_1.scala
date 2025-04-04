import scala.quoted.*

object Macros {
  def valuesImpl[A: Type](using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val symbol = TypeRepr.of[A].typeSymbol.fieldMember("value")
    Ref(symbol).asExprOf[Any]
  }

  transparent inline def values[A]: Any = ${ valuesImpl[A] }
}
