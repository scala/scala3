package scala.quoted

/** Lift expressions */
object Lifted {

  /** Lift a value into an expression containing the construction of that value */
  def apply[T](x: T)(using qctx: QuoteContext, lift: Liftable[T]): Expr[T] =
    lift.toExpr(x)

}
