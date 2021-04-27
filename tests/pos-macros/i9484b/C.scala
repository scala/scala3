import scala.quoted.*

object C {
  inline def m: Any = ${ mExpr }
  def mExpr(using Quotes): Expr[Any] = '{ () }
}
