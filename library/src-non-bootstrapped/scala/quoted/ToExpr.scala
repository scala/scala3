package scala.quoted

trait ToExpr[T]:
  def apply(x: T)(using Quotes): Expr[T]
