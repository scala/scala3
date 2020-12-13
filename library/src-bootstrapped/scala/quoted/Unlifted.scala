package scala.quoted

@deprecated("Use `scala.quoted.Expr` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
object Unlifted:

  @deprecated("Use `scala.quoted.Expr.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def apply[T](expr: Expr[T])(using FromExpr[T])(using Quotes): Option[T] =
    Expr.unapply(expr)

  @deprecated("Use `scala.quoted.Exprs.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def unapply[T](exprs: Seq[Expr[T]])(using FromExpr[T])(using Quotes): Option[Seq[T]] =
    Exprs.unapply(exprs)

end Unlifted
