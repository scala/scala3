import scala.quoted._

object Q {
  inline def f(): Any = ${ fExpr }
  def fExpr(using QuoteContext): Expr[Any] = { new LC; Expr(1) }
}