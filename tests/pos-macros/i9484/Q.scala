import scala.quoted._

object Q {
  inline def f(): Any = ${ fExpr }
  def fExpr(using s: Scope): s.Expr[Any] = { new LC; Expr(1) }
}