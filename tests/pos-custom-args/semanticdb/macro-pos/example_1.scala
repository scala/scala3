import quoted._

object CodeImpl {
  def codeExpr(using s: Scope): s.Expr[String] = '{""}
}
