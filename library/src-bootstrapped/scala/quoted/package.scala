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
    given autoToExpr[T] as Conversion[T, Expr[T]] given Liftable[T], QuoteContext = _.toExpr
  }

  implicit object ExprOps {
    def (x: T) toExpr[T: Liftable] given QuoteContext: Expr[T] = the[Liftable[T]].toExpr(x)

   /** Lifts this sequence of expressions into an expression of a sequence
    *
    *  Transforms a sequence of expression
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

     /** Lifts this sequence of expressions into an expression of a tuple
     *
     *  Transforms a sequence of expression
     *    `Seq(e1, e2, ...)` where `ei: Expr[_]`
     *  to an expression equivalent to
     *    `'{ ($e1, $e2, ...) }` typed as an `Expr[Tuple]`
     */
    def (seq: Seq[Expr[_]]) toExprOfTuple given QuoteContext: Expr[Tuple] = {
      seq.size match {
        case 0  => Expr.unitExpr
        case 1  => '{ Tuple1( ${seq(0)}) }
        case 2  => '{ Tuple2( ${seq(0)}, ${seq(1)}) }
        case 3  => '{ Tuple3( ${seq(0)}, ${seq(1)}, ${seq(2)}) }
        case 4  => '{ Tuple4( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}) }
        case 5  => '{ Tuple5( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}) }
        case 6  => '{ Tuple6( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}) }
        case 7  => '{ Tuple7( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}) }
        case 8  => '{ Tuple8( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}) }
        case 9  => '{ Tuple9( ${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}) }
        case 10 => '{ Tuple10(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}) }
        case 11 => '{ Tuple11(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}) }
        case 12 => '{ Tuple12(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}) }
        case 13 => '{ Tuple13(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}) }
        case 14 => '{ Tuple14(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}) }
        case 15 => '{ Tuple15(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}) }
        case 16 => '{ Tuple16(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}) }
        case 17 => '{ Tuple17(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}) }
        case 18 => '{ Tuple18(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}, ${seq(17)}) }
        case 19 => '{ Tuple19(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}, ${seq(17)}, ${seq(18)}) }
        case 20 => '{ Tuple20(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}, ${seq(17)}, ${seq(18)}, ${seq(19)}) }
        case 21 => '{ Tuple21(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}, ${seq(17)}, ${seq(18)}, ${seq(19)}, ${seq(20)}) }
        case 22 => '{ Tuple22(${seq(0)}, ${seq(1)}, ${seq(2)}, ${seq(3)}, ${seq(4)}, ${seq(5)}, ${seq(6)}, ${seq(7)}, ${seq(8)}, ${seq(9)}, ${seq(10)}, ${seq(11)}, ${seq(12)}, ${seq(13)}, ${seq(14)}, ${seq(15)}, ${seq(16)}, ${seq(17)}, ${seq(18)}, ${seq(19)}, ${seq(20)}, ${seq(21)}) }
        case _  => '{ Tuple.fromIArray(IArray(${seq.toExprOfSeq}: _*)) }
      }

    }
  }

}
