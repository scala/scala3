package scala

package object quoted {

  /** Evaluate the contents of this expression and return the result.
   *  It provides a new QuoteContext that is only valid within the scope the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = run { // (given qctx: QuoteContext) =>
   *    expr
   *  }
   *  ```
   *  where `expr: Expr[T]`
   *
   *  This method shoul not be called in a context where there is already has a QuoteContext.
   *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  def run[T](expr: given QuoteContext => Expr[T]) given (toolbox: Toolbox): T = toolbox.run(expr given _)

  /** Provide a new quote context within the scope of the argument that is only valid within the scope the argument.
   *  Return the result of the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = withQuoteContext { // (given qctx: QuoteContext) =>
   *    thunk
   *  }
   *  ```
   *  where `thunk: T`
   *
   *  This method shoul not be called in a context where there is already has a QuoteContext.
   */
  def withQuoteContext[T](thunk: given QuoteContext => T) given (toolbox: Toolbox): T = {
    var result: T = NoResult.asInstanceOf[T]
    def dummyRun given QuoteContext: Expr[Unit] = {
      result = thunk
      '{}
    }
    toolbox.run(dummyRun given _)
    assert(result != NoResult) // toolbox.run should have thrown an exception
    result
  }

  private object NoResult

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
