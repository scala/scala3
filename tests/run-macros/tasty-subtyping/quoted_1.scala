import scala.quoted._

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ${isTypeEqualImpl(Type[T], Type[U])}

  inline def isSubTypeOf[T, U]: Boolean =
    ${isSubTypeOfImpl(Type[T], Type[U])}

  def isTypeEqualImpl[T, U](t: Type[T], u: Type[U])(using QuoteContext) : Expr[Boolean] = {
    import qctx.tasty._
    val isTypeEqual = t.unseal.tpe =:= u.unseal.tpe
    Expr(isTypeEqual)
  }

  def isSubTypeOfImpl[T, U](t: Type[T], u: Type[U])(using QuoteContext) : Expr[Boolean] = {
    import qctx.tasty._
    val isTypeEqual = t.unseal.tpe <:< u.unseal.tpe
    Expr(isTypeEqual)
  }
}
