package scala

package object quoted {

  implicit object ExprOps {
    def (x: T) toExpr[T: Liftable](given QuoteContext): Expr[T] = summon[Liftable[T]].toExpr(x)
  }
}
