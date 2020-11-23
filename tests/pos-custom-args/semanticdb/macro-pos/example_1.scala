import quoted._

object CodeImpl {
  def codeExpr(using Quotes): Expr[String] = '{""}
}
