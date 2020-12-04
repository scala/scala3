package scala.quoted

trait FromExpr[T]:
  def unapply(x: Expr[T])(using Quotes): Option[T]
