package scala.quoted

/** Implicit conversion from a term of type `T` to an expression of type `Expr[T]` */
given autolift[T](using Liftable[T], QuoteContext) as Conversion[T, Expr[T]] = Expr(_)
