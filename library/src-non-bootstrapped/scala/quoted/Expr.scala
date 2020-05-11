package scala.quoted

class Expr[+T] private[scala]:
  def unseal(using qctx: QuoteContext): qctx.tasty.Term =
    throw new Exception("Non bootstrapped library")
