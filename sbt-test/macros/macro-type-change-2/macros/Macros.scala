package Macros

import scala.quoted.*

object Macros {
  inline def hasAnyField[T]: Boolean = ${ hasAnyFieldImpl[T] }

  def hasAnyFieldImpl[T: Type](using Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    val hasField = TypeRepr.of[T].typeSymbol.fieldMembers.nonEmpty

    Expr(hasField)
  }
}
