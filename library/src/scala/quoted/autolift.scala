package scala.quoted

/** Enable implicit conversion from a term of type `T` to an expression of type `Expr[T]` */
object autolift {
  /** Implicit conversion from a term of type `T` to an expression of type `Expr[T]` */
  given autoToExpr[T](using Liftable[T], QuoteContext) as Conversion[T, Expr[T]] = Expr(_)
}
