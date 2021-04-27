import scala.quoted.*

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ${isTypeEqualImpl[T, U]}

  inline def isSubTypeOf[T, U]: Boolean =
    ${isSubTypeOfImpl[T, U]}

  def isTypeEqualImpl[T: Type, U: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    val isTypeEqual = TypeRepr.of[T] =:= TypeRepr.of[U]
    Expr(isTypeEqual)
  }

  def isSubTypeOfImpl[T: Type, U: Type](using Quotes) : Expr[Boolean] = {
    import quotes.reflect.*
    val isTypeEqual = TypeRepr.of[T] <:< TypeRepr.of[U]
    Expr(isTypeEqual)
  }
}
