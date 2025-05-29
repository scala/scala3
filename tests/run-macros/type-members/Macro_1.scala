package example

import scala.quoted.*

object Macro {
  inline def typeMembers[T <: AnyKind]: String = ${ typeMembersImpl[T] }

  def typeMembersImpl[T <: AnyKind: Type](using quotes: Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(s"${TypeRepr.of[T].typeSymbol}: ${TypeRepr.of[T].typeSymbol.typeMembers.toString}")
  }
}
