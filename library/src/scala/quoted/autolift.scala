package scala.quoted

/** Enable implicit conversion from a term of type `T` to an expression of type `Expr[T]` */
object autolift {
  /** Implicit conversion from a term of type `T` to an expression of type `Expr[T]` */
  given autoToExpr[T](given Liftable[T], QuoteContext): Conversion[T, Expr[T]] = Expr(_)
}
