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
    !scala.internal.quoted.Expr.unapply[EmptyTuple, EmptyTuple](this)(using that, qctx).isEmpty

  /** Checked cast to a `quoted.Expr[U]` */
  def cast[U](using tp: scala.quoted.Type[U])(using qctx: QuoteContext): scala.quoted.Expr[U] = asExprOf[U]

  /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
  def isExprOf[X](using tp: scala.quoted.Type[X])(using qctx: QuoteContext): Boolean =
    this.unseal.tpe <:< tp.unseal.tpe

  /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
  def asExprOf[X](using tp: scala.quoted.Type[X])(using qctx: QuoteContext): scala.quoted.Expr[X] = {
    if isExprOf[X] then
      this.asInstanceOf[scala.quoted.Expr[X]]
    else
      throw new tasty.reflect.ExprCastError(
        s"""Expr: ${this.show}
           |of type: ${this.unseal.tpe.show}
           |did not conform to type: ${tp.unseal.tpe.show}
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
    seq match {
      case Seq() =>
        '{ Tuple() }
      case Seq('{ $x1: $t1 }) =>
        '{ Tuple1($x1) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }) =>
        '{ Tuple2($x1, $x2) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }) =>
        '{ Tuple3($x1, $x2, $x3) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }) =>
        '{ Tuple4($x1, $x2, $x3, $x4) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }) =>
        '{ Tuple5($x1, $x2, $x3, $x4, $x5) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }) =>
        '{ Tuple6($x1, $x2, $x3, $x4, $x5, $x6) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }) =>
        '{ Tuple7($x1, $x2, $x3, $x4, $x5, $x6, $x7) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }) =>
        '{ Tuple8($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }) =>
        '{ Tuple9($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }) =>
        '{ Tuple10($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }) =>
        '{ Tuple11($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }) =>
        '{ Tuple12($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }) =>
        '{ Tuple13($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }) =>
        '{ Tuple14($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }) =>
        '{ Tuple15($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }) =>
        '{ Tuple16($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }) =>
        '{ Tuple17($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }, '{ $x18: $t18 }) =>
        '{ Tuple18($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }, '{ $x18: $t18 }, '{ $x19: $t19 }) =>
        '{ Tuple19($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }, '{ $x18: $t18 }, '{ $x19: $t19 }, '{ $x20: $t20 }) =>
        '{ Tuple20($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }, '{ $x18: $t18 }, '{ $x19: $t19 }, '{ $x20: $t20 }, '{ $x21: $t21 }) =>
        '{ Tuple21($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21) }
      case Seq('{ $x1: $t1 }, '{ $x2: $t2 }, '{ $x3: $t3 }, '{ $x4: $t4 }, '{ $x5: $t5 }, '{ $x6: $t6 }, '{ $x7: $t7 }, '{ $x8: $t8 }, '{ $x9: $t9 }, '{ $x10: $t10 }, '{ $x11: $t11 }, '{ $x12: $t12 }, '{ $x13: $t13 }, '{ $x14: $t14 }, '{ $x15: $t15 }, '{ $x16: $t16 }, '{ $x17: $t17 }, '{ $x18: $t18 }, '{ $x19: $t19 }, '{ $x20: $t20 }, '{ $x21: $t21 }, '{ $x22: $t22 }) =>
        '{ Tuple22($x1, $x2, $x3, $x4, $x5, $x6, $x7, $x8, $x9, $x10, $x11, $x12, $x13, $x14, $x15, $x16, $x17, $x18, $x19, $x20, $x21, $x22) }
      case _ =>
        '{ Tuple.fromIArray(IArray(${Varargs(seq)}: _*)) }
    }
  }

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
    Implicits.search(tpe.unseal.tpe) match {
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
