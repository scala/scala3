package scala.internal.quoted

/** Exception thrown when an `Expr[?]` is casted to a `Expr[U]` and the expression is not of type `U` */
private[scala] class ExprCastError(msg: String) extends Throwable(msg)
