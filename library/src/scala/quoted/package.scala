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
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
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
   *  This method should not be called in a context where there is already has a `QuoteContext`
   *  such as within a `run` or a `withQuoteContext`.
   */
  def withQuoteContext[T](thunk: given QuoteContext => T) given (toolbox: Toolbox): T = {
    var result: T = NoResult.asInstanceOf[T]
    def dummyRun given QuoteContext: Expr[Unit] = {
      result = thunk
      Expr.unitExpr
    }
    toolbox.run(dummyRun given _)
    assert(result != NoResult) // toolbox.run should have thrown an exception
    result
  }

  private object NoResult

  object autolift {
    implicit def autoToExpr[T: Liftable](x: T) given QuoteContext: Expr[T] = x.toExpr
  }

  implicit object ExprOps {
    def (x: T) toExpr[T: Liftable] given QuoteContext: Expr[T] = the[Liftable[T]].toExpr(x)

   /** Lifts this sequence of expressions into an expression of a sequence
    *
    *  Transforms a list of expression
    *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
    *  to an expression equivalent to
    *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
    *
    *  Usage:
    *  ```scala
    *  '{ List(${List(1, 2, 3).toExprOfSeq}: _*) } // equvalent to '{ List(1, 2, 3) }
    *  ```
    */
    def (seq: Seq[Expr[T]]) toExprOfSeq[T] given (tp: Type[T], qctx: QuoteContext): Expr[Seq[T]] = {
      import qctx.tasty._
      Repeated(seq.map(_.unseal).toList, tp.unseal).seal.asInstanceOf[Expr[Seq[T]]]
    }

    /** Lifts this list of expressions into an expression of a list
     *
     *  Transforms a list of expression
     *    `List(e1, e2, ...)` where `ei: Expr[T]`
     *  to an expression equivalent to
     *    `'{ List($e1, $e2, ...) }` typed as an `Expr[List[T]]`
     */
    def (list: List[Expr[T]]) toExprOfList[T] given Type[T], QuoteContext: Expr[List[T]] =
      if (list.isEmpty) '{ Nil } else '{ List(${list.toExprOfSeq}: _*) }


  }

}
