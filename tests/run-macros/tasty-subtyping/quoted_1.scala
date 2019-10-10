import scala.quoted._
import scala.quoted.autolift.given

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ${isTypeEqualImpl('[T], '[U])}

  inline def isSubTypeOf[T, U]: Boolean =
    ${isSubTypeOfImpl('[T], '[U])}

  def isTypeEqualImpl[T, U](t: TypeTag[T], u: TypeTag[U])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty.{_, given}
    val isTypeEqual = t.unseal.tpe =:= u.unseal.tpe
    isTypeEqual
  }

  def isSubTypeOfImpl[T, U](t: TypeTag[T], u: TypeTag[U])(given qctx: QuoteContext): Expr[Boolean] = {
    import qctx.tasty.{_, given}
    val isTypeEqual = t.unseal.tpe <:< u.unseal.tpe
    isTypeEqual
  }
}
