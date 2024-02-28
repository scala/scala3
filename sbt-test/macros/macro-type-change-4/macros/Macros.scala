package Macros

import scala.quoted.*

object Macros {
  inline def hasAnyField[T](placeholder: Boolean): Boolean = ${ hasAnyFieldImpl[T]('placeholder) }

  def hasAnyFieldImpl[T: Type](placeholder: Expr[Boolean])(using Quotes): Expr[Boolean] = {
    import quotes.reflect.*

    val hasField = TypeRepr.of[T].typeSymbol.declaredFields.nonEmpty

    Expr(hasField)
  }
}
