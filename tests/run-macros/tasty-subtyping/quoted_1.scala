import scala.quoted._

object Macros {

  inline def isTypeEqual[T, U]: Boolean =
    ${isTypeEqualImpl('[T], '[U])}

  inline def isSubTypeOf[T, U]: Boolean =
    ${isSubTypeOfImpl('[T], '[U])}

  def isTypeEqualImpl[T, U](using s: Scope)(t: s.Type[T], u: s.Type[U]): s.Expr[Boolean] = {
    import s.tasty._
    val isTypeEqual = t.tpe =:= u.tpe
    Expr(isTypeEqual)
  }

  def isSubTypeOfImpl[T, U](using s: Scope)(t: s.Type[T], u: s.Type[U]): s.Expr[Boolean] = {
    import s.tasty._
    val isTypeEqual = t.tpe <:< u.tpe
    Expr(isTypeEqual)
  }
}
