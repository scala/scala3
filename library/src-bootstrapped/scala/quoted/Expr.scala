package scala.quoted

/** Quoted expression of type `T` */
abstract class Expr[+T] private[scala] {

  /** Show a source code like representation of this expression without syntax highlight */
  def show(using qctx: QuoteContext): String = this.unseal.show

  /** Shows the tree as fully typed source code colored with ANSI */
  def showAnsiColored(using qctx: QuoteContext): String = this.unseal.showAnsiColored

  /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
   *  It does the equivalent of
   *  ```
   *  this match
   *    case '{...} => true // where the contents of the pattern are the contents of `that`
   *    case _ => false
   *  ```
   */
  final def matches(that: Expr[Any])(using qctx: QuoteContext): Boolean =
    val ExprMatch = qctx.asInstanceOf[scala.internal.quoted.QuoteContextInternal].ExprMatch
    ExprMatch.unapply[EmptyTuple, EmptyTuple](this)(using that).nonEmpty

  /** Checked cast to a `quoted.Expr[U]` */
  def cast[U](using tp: scala.quoted.Type[U])(using qctx: QuoteContext): scala.quoted.Expr[U] = asExprOf[U]

  /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
  def isExprOf[X](using tp: scala.quoted.Type[X])(using qctx: QuoteContext): Boolean =
    this.unseal.tpe <:< qctx.reflect.TypeRepr.of[X]

  /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
  def asExprOf[X](using tp: scala.quoted.Type[X])(using qctx: QuoteContext): scala.quoted.Expr[X] = {
    if isExprOf[X] then
      this.asInstanceOf[scala.quoted.Expr[X]]
    else
      throw Exception(
        s"""Expr cast exception: ${this.show}
           |of type: ${this.unseal.tpe.show}
           |did not conform to type: ${qctx.reflect.TypeRepr.of[X].show}
           |""".stripMargin
      )
  }

  /** View this expression `quoted.Expr[T]` as a `Term` */
  def unseal(using qctx: QuoteContext): qctx.reflect.Term

}

object Expr {

  extension [T](expr: Expr[T]):
    /** Return the unlifted value of this expression.
     *
     *  Returns `None` if the expression does not contain a value or contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    def unlift(using qctx: QuoteContext, unlift: Unliftable[T]): Option[T] =
      unlift.fromExpr(expr)

    /** Return the unlifted value of this expression.
     *
     *  Emits an error and throws if the expression does not contain a value or contains side effects.
     *  Otherwise returns the value.
     */
    def unliftOrError(using qctx: QuoteContext, unlift: Unliftable[T]): T =
      def reportError =
        val msg = s"Expected a known value. \n\nThe value of: ${expr.show}\ncould not be unlifted using $unlift"
        report.throwError(msg, expr)
      unlift.fromExpr(expr).getOrElse(reportError)
  end extension

  /** `e.betaReduce` returns an expression that is functionally equivalent to `e`,
   *   however if `e` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
   *   then it optimizes this the top most call by returning the result of beta-reducing the application.
   *   Otherwise returns `expr`.
   *
   *   To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
   *   Some bindings may be elided as an early optimization.
   */
  def betaReduce[T](expr: Expr[T])(using qctx: QuoteContext): Expr[T] =
    qctx.reflect.Term.betaReduce(expr.unseal) match
      case Some(expr1) => expr1.seal.asInstanceOf[Expr[T]]
      case _ => expr

  /** Returns an expression containing a block with the given statements and ending with the expresion
   *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
   *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
   */
  def block[T](statements: List[Expr[Any]], expr: Expr[T])(using qctx: QuoteContext): Expr[T] = {
    import qctx.reflect._
    Block(statements.map(_.unseal), expr.unseal).seal.asInstanceOf[Expr[T]]
  }

  /** Lift a value into an expression containing the construction of that value */
  def apply[T](x: T)(using qctx: QuoteContext, lift: Liftable[T]): Expr[T] = lift.toExpr(x)

  /** Lifts this sequence of expressions into an expression of a sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   *  ```
   */
  def ofSeq[T](xs: Seq[Expr[T]])(using tp: Type[T], qctx: QuoteContext): Expr[Seq[T]] = Varargs(xs)

  /** Lifts this list of expressions into an expression of a list
   *
   *  Transforms a list of expression
   *    `List(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ List($e1, $e2, ...) }` typed as an `Expr[List[T]]`
   */
  def  ofList[T](xs: Seq[Expr[T]])(using Type[T], QuoteContext): Expr[List[T]] =
    if (xs.isEmpty) Expr(Nil) else '{ List(${Varargs(xs)}: _*) }

  /** Lifts this sequence of expressions into an expression of a tuple
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[Any]`
   *  to an expression equivalent to
   *    `'{ ($e1, $e2, ...) }` typed as an `Expr[Tuple]`
   */
  def ofTupleFromSeq(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] = {
    seq.size match {
      case 0 => '{ Tuple() }
      case 1 => ofTupleFromSeq1(seq)
      case 2 => ofTupleFromSeq2(seq)
      case 3 => ofTupleFromSeq3(seq)
      case 4 => ofTupleFromSeq4(seq)
      case 5 => ofTupleFromSeq5(seq)
      case 6 => ofTupleFromSeq6(seq)
      case 7 => ofTupleFromSeq7(seq)
      case 8 => ofTupleFromSeq8(seq)
      case 9 => ofTupleFromSeq9(seq)
      case 10 => ofTupleFromSeq10(seq)
      case 11 => ofTupleFromSeq11(seq)
      case 12 => ofTupleFromSeq12(seq)
      case 13 => ofTupleFromSeq13(seq)
      case 14 => ofTupleFromSeq14(seq)
      case 15 => ofTupleFromSeq15(seq)
      case 16 => ofTupleFromSeq16(seq)
      case 17 => ofTupleFromSeq17(seq)
      case 18 => ofTupleFromSeq18(seq)
      case 19 => ofTupleFromSeq19(seq)
      case 20 => ofTupleFromSeq20(seq)
      case 21 => ofTupleFromSeq21(seq)
      case 22 => ofTupleFromSeq22(seq)
      case _ => '{ Tuple.fromIArray(IArray(${Varargs(seq)}: _*)) }
    }
  }

  private def ofTupleFromSeq1(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }) => '{ Tuple1($x1) }

  private def ofTupleFromSeq2(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }) => '{ Tuple2($x1, $x2) }

  private def ofTupleFromSeq3(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }) => '{ Tuple3($x1, $x2, $x3) }

  private def ofTupleFromSeq4(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }) =>
       '{ Tuple4($x1, $x2, $x3, $x4) }

  private def ofTupleFromSeq5(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }) =>
        '{ Tuple5($x1, $x2, $x3, $x4, $x5) }

  private def ofTupleFromSeq6(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }) =>
        '{ Tuple6($x1, $x2, $x3, $x4, $x5, $x6) }

  private def ofTupleFromSeq7(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }) =>
        '{ Tuple7($x1, $x2, $x3, $x4, $x5, $x6, $x7) }

  private def ofTupleFromSeq8(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }) =>
        '{ Tuple8($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8) }

  private def ofTupleFromSeq9(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }) =>
        '{ Tuple9($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9) }

  private def ofTupleFromSeq10(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }) =>
        '{ Tuple10($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10) }

  private def ofTupleFromSeq11(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }) =>
        '{ Tuple11($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11) }

  private def ofTupleFromSeq12(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }) =>
        '{ Tuple12($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12) }

  private def ofTupleFromSeq13(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }) =>
        '{ Tuple13($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13) }

  private def ofTupleFromSeq14(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }) =>
        '{ Tuple14($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14) }

  private def ofTupleFromSeq15(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }) =>
        '{ Tuple15($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15) }

  private def ofTupleFromSeq16(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }) =>
        '{ Tuple16($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16) }

  private def ofTupleFromSeq17(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }) =>
        '{ Tuple17($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17) }

  private def ofTupleFromSeq18(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }) =>
        '{ Tuple18($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18) }

  private def ofTupleFromSeq19(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }) =>
        '{ Tuple19($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19) }

  private def ofTupleFromSeq20(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }) =>
        '{ Tuple20($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20) }

  private def ofTupleFromSeq21(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }, '{ $x21: t21 }) =>
        '{ Tuple21($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21) }

  private def ofTupleFromSeq22(seq: Seq[Expr[Any]])(using qctx: QuoteContext): Expr[Tuple] =
    seq match
      case Seq('{ $x1: t1 }, '{ $x2: t2 }, '{ $x3: t3 }, '{ $x4: t4 }, '{ $x5: t5 }, '{ $x6: t6 }, '{ $x7: t7 }, '{ $x8: t8 }, '{ $x9: t9 }, '{ $x10: t10 }, '{ $x11: t11 }, '{ $x12: t12 }, '{ $x13: t13 }, '{ $x14: t14 }, '{ $x15: t15 }, '{ $x16: t16 }, '{ $x17: t17 }, '{ $x18: t18 }, '{ $x19: t19 }, '{ $x20: t20 }, '{ $x21: t21 }, '{ $x22: t22 }) =>
        '{ Tuple22($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21, $x22) }


  /** Given a tuple of the form `(Expr[A1], ..., Expr[An])`, outputs a tuple `Expr[(A1, ..., An)]`. */
  def ofTuple[T <: Tuple: Tuple.IsMappedBy[Expr]: Type](tup: T)(using qctx: QuoteContext): Expr[Tuple.InverseMap[T, Expr]] = {
    val elems: Seq[Expr[Any]] = tup.asInstanceOf[Product].productIterator.toSeq.asInstanceOf[Seq[Expr[Any]]]
    ofTupleFromSeq(elems).asExprOf[Tuple.InverseMap[T, Expr]]
  }

  /** Find a given instance of type `T` in the current scope.
   *  Return `Some` containing the expression of the implicit or
   * `None` if implicit resolution failed.
   *
   *  @tparam T type of the implicit parameter
   *  @param tpe quoted type of the implicit parameter
   *  @param qctx current context
   */
  def summon[T](using tpe: Type[T])(using qctx: QuoteContext): Option[Expr[T]] = {
    import qctx.reflect._
    Implicits.search(TypeRepr.of[T]) match {
      case iss: ImplicitSearchSuccess => Some(iss.tree.seal.asInstanceOf[Expr[T]])
      case isf: ImplicitSearchFailure => None
    }
  }

  object StringContext {
    /** Matches a `StringContext(part0, part1, ...)` and extracts the parts of a call to if the
     *  parts are passed explicitly. Returns the equvalent to `Seq('{part0}, '{part1}, ...)`.
     */
    def unapply(sc: Expr[StringContext])(using QuoteContext): Option[Seq[Expr[String]]] =
      sc match
        case '{ scala.StringContext(${Varargs(parts)}: _*) } => Some(parts)
        case '{ new scala.StringContext(${Varargs(parts)}: _*) } => Some(parts)
        case _ => None
  }

}
