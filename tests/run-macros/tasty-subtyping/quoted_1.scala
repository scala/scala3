import scala.quoted._

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ${isTypeEqualImpl('[T], '[U])}

  inline def isSubTypeOf[T, U]: Boolean =
    ${isSubTypeOfImpl('[T], '[U])}

  def isTypeEqualImpl[T, U](t: Type[T], u: Type[U])(using QuoteContext) : Expr[Boolean] = {
    import qctx.reflect._
    val isTypeEqual = t.unseal.tpe =:= u.unseal.tpe
    Expr(isTypeEqual)
  }

  def isSubTypeOfImpl[T, U](t: Type[T], u: Type[U])(using QuoteContext) : Expr[Boolean] = {
    import qctx.reflect._
    val isTypeEqual = t.unseal.tpe <:< u.unseal.tpe
    Expr(isTypeEqual)
  }
}
