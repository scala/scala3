package scala.quoted

import scala.quoted.show.SyntaxHighlight
import scala.internal.tasty.CompilerInterface.quoteContextWithCompilerInterface

object Expr {

  /** `e.betaReduce` returns an expression that is functionally equivalent to `e`,
   *   however if `e` is of the form `((y1, ..., yn) => e2)(x1, ..., xn)`
   *   then it optimizes this the top most call by returning the result of beta-reducing the application.
   *   Otherwise returns `expr`.
   */
  def betaReduce[T](using s: Scope)(expr: s.Expr[T]): s.Expr[T] =
    val s1 = quoteContextWithCompilerInterface(s)
    s1.tasty.betaReduce(expr) match
      case Some(expr1) => expr1.seal.asInstanceOf[s.Expr[T]]
      case _ => expr

  /** Returns an expression containing a block with the given statements and ending with the expresion
   *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
   *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
   */
  def block[T](using s: Scope)(statements: List[s.Expr[Any]], expr: s.Expr[T]): s.Expr[T] = {
    import s.tasty._
    Block(statements, expr).seal.asInstanceOf[s.Expr[T]]
  }

  /** Lift a value into an expression containing the construction of that value */
  def apply[T](x: T)(using s: Scope)(using lift: s.Liftable[T]): s.Expr[T] = lift.toExpr(x)

  /** Lifts this sequence of expressions into an expression of a sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   *  ```
   */
  def ofSeq[T](using s: Scope)(xs: Seq[s.Expr[T]])(using s.Type[T]): s.Expr[Seq[T]] = Varargs(xs)

  /** Lifts this list of expressions into an expression of a list
   *
   *  Transforms a list of expression
   *    `List(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ List($e1, $e2, ...) }` typed as an `Expr[List[T]]`
   */
  def  ofList[T](using s: Scope)(xs: Seq[s.Expr[T]])(using s.Type[T]): s.Expr[List[T]] =
    if (xs.isEmpty) Expr(Nil) else '{ List(${Varargs(xs)}: _*) }

  /** Lifts this sequence of expressions into an expression of a tuple
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[Any]`
   *  to an expression equivalent to
   *    `'{ ($e1, $e2, ...) }` typed as an `Expr[Tuple]`
   */
  def ofTupleFromSeq(using s: Scope)(seq: Seq[s.Expr[Any]]): s.Expr[Tuple] = {
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
  def ofTuple[T <: Tuple](tup: T)(using s: Scope)(using Tuple.IsMappedBy[s.Expr][T]): s.Expr[Tuple.InverseMap[T, s.Expr]] = {
    val elems: Seq[s.Expr[Any]] = tup.productIterator.toSeq.asInstanceOf[Seq[s.Expr[Any]]]
    ofTupleFromSeq(elems).asInstanceOf[s.Expr[Tuple.InverseMap[T, s.Expr]]]
  }

  /** Find an implicit of type `T` in the current scope given by `s`.
   *  Return `Some` containing the expression `s.Expr[T]` of the implicit or
   * `None` if implicit resolution failed.
   *
   *  @tparam T type of the implicit parameter
   *  @param tpe quoted type of the implicit parameter
   *  @param s current Scope
   */
  def summon[T](using s: Scope)(using tpe: s.Type[T]): Option[s.Expr[T]] = {
    import s.tasty._
    searchImplicit(tpe.tpe) match {
      case iss: ImplicitSearchSuccess => Some(iss.tree.seal.asInstanceOf[s.Expr[T]])
      case isf: ImplicitSearchFailure => None
    }
  }

  object StringContext {
    /** Matches a `StringContext(part0, part1, ...)` and extracts the parts of a call to if the
     *  parts are passed explicitly. Returns the equvalent to `Seq('{part0}, '{part1}, ...)`.
     */
    def unapply(using s: Scope)(sc: s.Expr[StringContext]): Option[Seq[s.Expr[String]]] =
      sc match
        case '{ scala.StringContext(${Varargs(parts)}: _*) } => Some(parts)
        case '{ new scala.StringContext(${Varargs(parts)}: _*) } => Some(parts)
        case _ => None
  }

}
