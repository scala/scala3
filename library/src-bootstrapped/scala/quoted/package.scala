package scala

package object quoted {

  implicit object ExprOps {
    @deprecated("Use scala.quoted.Expr.apply instead", "0.19.0")
    def (x: T) toExpr[T: Liftable](given QuoteContext): Expr[T] = Expr(x)
  }
}
