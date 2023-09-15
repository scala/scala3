object Macro {
  import scala.quoted.*

  def subtypesImpl[A: Type](using quotes: Quotes): Expr[String] = {
    import quotes.reflect.*
    val a = TypeRepr.of[A].typeSymbol.children
    '{""}
  }

  inline def subtypes[A]: String = ${ subtypesImpl[A] }
}
