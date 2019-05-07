package scala

package object quoted {

  type Staged[T] = /*given*/ StagingContext => Expr[T]

  type StagedType[T] = /*given*/ StagingContext => Type[T]

  implicit class LiftExprOps[T](val x: T) extends AnyVal {
    def toExpr(implicit ev: Liftable[T], st: StagingContext): Expr[T] = ev.toExpr(x)
  }

}
