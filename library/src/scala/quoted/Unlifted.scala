package scala.quoted

@deprecated("Use `scala.quoted.Values` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
object Unlifted:

  @deprecated("Use `scala.quoted.Values.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def apply[T](expr: Expr[T])(using FromExpr[T])(using Quotes): Option[T] =
    Value.unapply(expr)

  @deprecated("Use `scala.quoted.Values.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def unapply[T](exprs: Seq[Expr[T]])(using FromExpr[T])(using Quotes): Option[Seq[T]] =
    Values.unapply(exprs)

end Unlifted
