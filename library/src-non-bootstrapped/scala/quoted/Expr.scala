package scala.quoted

abstract class Expr[+T] private[scala]:
  def unseal(using qctx: QuoteContext): qctx.tasty.Term = asTerm
  def asTerm(using qctx: QuoteContext): qctx.tasty.Term
  def asExprOf[U](using tp: scala.quoted.Type[U])(using qctx: QuoteContext): scala.quoted.Expr[U] =
    throw new Exception("Non bootstrapped lib")
