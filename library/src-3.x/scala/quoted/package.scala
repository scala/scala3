package scala

package object quoted {

  type Staged[T] = implicit StagingContext => Expr[T]

  type StagedType[T] = implicit StagingContext => Type[T]

  object autolift {
    implicit def autoToExpr[T: Liftable](x: T): Expr[T] = x.toExpr
  }

  implicit class LiftExprOps[T](val x: T) extends AnyVal {
    def toExpr(implicit ev: Liftable[T], st: StagingContext): Expr[T] = ev.toExpr(x)
  }

  implicit class ListOfExprOps[T](val list: List[Expr[T]]) extends AnyVal {
    def toExprOfList(implicit ev: Type[T], st: StagingContext): Expr[List[T]] = {
      def rec(list: List[Expr[T]]): Expr[List[T]] = list match {
        case x :: xs  => '{ ($x) :: ${rec(xs)} }
        case Nil => '{Nil}
      }
      rec(list)
    }
  }
}
