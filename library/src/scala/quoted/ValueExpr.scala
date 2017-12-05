package scala.quoted

final class ValueExpr[T <: AnyVal](val value: T) extends Expr[T]
