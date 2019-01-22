package scala

package object quoted {

  object autolift {
    implicit def autoToExpr[T: Liftable](x: T): Expr[T] = x.toExpr
  }

  implicit class LiftExprOps[T](val x: T) extends AnyVal {
    def toExpr(implicit ev: Liftable[T]): Expr[T] = ev.toExpr(x)
  }

  implicit class ListOfExprOps[T](val list: List[Expr[T]]) extends AnyVal {
    def toExprOfList(implicit ev: Type[T]): Expr[List[T]] = {
      def rec(list: List[Expr[T]]): Expr[List[T]] = list match {
        case x :: xs  => '{ ($x) :: ${rec(xs)} }
        case Nil => '{Nil}
      }
      rec(list)
    }
  }
}
