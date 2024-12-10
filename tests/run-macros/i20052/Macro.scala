import scala.quoted.*

object Macro {
  inline def logPrimaryConstructor[A]: String = ${ logPrimaryConstructorImpl[A] }

  def logPrimaryConstructorImpl[A](using Type[A], Quotes): Expr[String] = {
    import quotes.reflect.*

    val primaryConstructor = TypeRepr.of[A].typeSymbol.primaryConstructor
    val flags = primaryConstructor.flags.show
    val paramSymss = primaryConstructor.paramSymss
    val clauses = paramSymss.map(_.map(param => (param.name, TypeRepr.of[A].memberType(param).show)))
    val str = s"${primaryConstructor} (${primaryConstructor.flags.show}) ${clauses}"
    Expr(str)
  }
}
