import quoted.*

object CodeImpl {
  def codeExpr(using Quotes): Expr[String] = '{""}
}
