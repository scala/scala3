package scala.quoted

trait Unliftable[T]:
  def fromExpr(x: Expr[T]): QuoteContext ?=> Option[T]
