import quoted._

object CodeImpl {
  def codeExpr(using qctx: QuoteContext): Expr[String] = '{""}
}
