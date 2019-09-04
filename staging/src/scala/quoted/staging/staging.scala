package scala.quoted

package object staging {

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
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
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
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
   */
  def withQuoteContext[T](thunk: given QuoteContext => T) given (toolbox: Toolbox): T = {
    val noResult = new Object
    var result: T = noResult.asInstanceOf[T]
    def dummyRun given QuoteContext: Expr[Unit] = {
      result = thunk
      Expr.unitExpr
    }
    toolbox.run(dummyRun given _)
    assert(result != noResult) // toolbox.run should have thrown an exception
    result
  }

}
