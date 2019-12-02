package scala

package object quoted {

  implicit object ExprOps {
    @deprecated("Use scala.quoted.Expr.apply instead", "0.19.0")
    def [T: Liftable](x: T) toExpr (given QuoteContext): Expr[T] = Expr(x)
  }
}
