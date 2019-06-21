package scala

package object quoted {

  /** Evaluate the contents of this expression and return the result.
   *
   *  Usage:
   *  ```
   *  val e: T = run {
   *   expr
   *  }
   *  ```
   *  where `expr: Expr[T]`
    *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  def run[T](expr: Expr[T]) given (toolbox: Toolbox): T = toolbox.run(expr)

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
