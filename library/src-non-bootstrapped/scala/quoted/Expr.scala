package scala.quoted

abstract class Expr[+T] private[scala]:
  def unseal(using qctx: QuoteContext): qctx.tasty.Term
