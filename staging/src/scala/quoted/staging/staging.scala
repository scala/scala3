package scala.quoted

package object staging:

  /** Evaluate the contents of this expression and return the result.
   *  It provides a new QuoteContext that is only valid within the scope the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = run { // (using qctx: QuoteContext) =>
   *    expr
   *  }
   *  ```
   *  where `expr: Expr[T]`
   *
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
   */
  def run[T](expr: QuoteContext ?=> Expr[T])(using toolbox: Toolbox): T = toolbox.run(expr(using _))

  /** Provide a new quote context within the scope of the argument that is only valid within the scope the argument.
   *  Return the result of the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = withQuoteContext { // (using qctx: QuoteContext) =>
   *    thunk
   *  }
   *  ```
   *  where `thunk: T`
   *
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
   */
  def withQuoteContext[T](thunk: QuoteContext ?=> T)(using toolbox: Toolbox): T =
    val noResult = new Object
    var result: T = noResult.asInstanceOf[T]
    def dummyRun(using QuoteContext): Expr[Unit] = {
      result = thunk
      '{}
    }
    toolbox.run(dummyRun(using _))
    assert(result != noResult) // toolbox.run should have thrown an exception
    result
  end withQuoteContext

end staging
