package scala.quoted

abstract class Expr[+T] private[scala]:
  def unseal(using qctx: QuoteContext): qctx.reflect.Term
  def asExprOf[X](using tp: scala.quoted.Type[X])(using qctx: QuoteContext): scala.quoted.Expr[X] = ???
