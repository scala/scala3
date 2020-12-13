import scala.quoted._

object Q {
  inline def f(): Any = ${ fExpr }
  def fExpr(using Quotes): Expr[Any] = { new LC; Expr(1) }
}