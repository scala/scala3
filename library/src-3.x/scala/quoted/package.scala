package scala

package object quoted {

  object autolift {
    implicit def autoToExpr[T: Liftable](x: T): Expr[T] = x.toExpr
  }

  implicit object ExprOps {
    def (x: T) toExpr[T] given Liftable[T]: Expr[T] = the[Liftable[T]].toExpr(x)

    def (list: List[Expr[T]]) toExprOfList[T] given Type[T]: Expr[List[T]] = list match {
      case x :: xs  => '{ $x :: ${xs.toExprOfList} }
      case Nil => '{ Nil }
    }
  }

}
