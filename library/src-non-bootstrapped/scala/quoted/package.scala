package scala

package object quoted {

  type Staged[T] = /*implicit*/ StagingContext => Expr[T]

  type StagedType[T] = /*implicit*/ StagingContext => Type[T]

  implicit class LiftExprOps[T](val x: T) extends AnyVal {
    def toExpr(implicit ev: Liftable[T], st: StagingContext): Expr[T] = ev.toExpr(x)
  }

}
