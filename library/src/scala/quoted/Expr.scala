package scala.quoted

import scala.quoted.show.SyntaxHighlight

/** Quoted expression of type `T`
 *
 *  Restriction: only the QuoteContext.tasty.internal implementation is allowed to extend this trait.
 *  Any other implementation will result in an undefined behavior.
 */
trait Expr[+T] {

  /** Show a source code like representation of this expression without syntax highlight */
  def show(implicit qctx: QuoteContext): String = qctx.show(this, SyntaxHighlight.plain)

  /** Show a source code like representation of this expression */
  def show(syntaxHighlight: SyntaxHighlight)(implicit qctx: QuoteContext): String = qctx.show(this, syntaxHighlight)

  /** Return the value of this expression.
   *
   *  Returns `None` if the expression does not contain a value or contains side effects.
   *  Otherwise returns the `Some` of the value.
   */
  final def getValue[U >: T](given qctx: QuoteContext, valueOf: ValueOfExpr[U]): Option[U] = valueOf(this)

  /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
   *  It does the equivalent of
   *  ```
   *  this match
   *    case '{...} => true // where the contens of the pattern are the contents of `that`
   *    case _ => false
   *  ```
   */
  final def matches(that: Expr[Any])(given qctx: QuoteContext): Boolean =
    !scala.internal.quoted.Expr.unapply[Unit, Unit](this)(given that, false, qctx).isEmpty

  /** Returns the undelying argument that was in the call before inlining.
   *
   *  ```
   *  inline foo(x: Int): Int = baz(x, x)
   *  foo(bar())
   *  ```
   *  is inlined as
   *  ```
   *  val x = bar()
   *  baz(x, x)
   *  ```
   *  in this case the undelying argument of `x` will be `bar()`.
   *
   *  Warning: Using the undelying argument directly in the expansion of a macro may change the parameter
   *           semantics from by-value to by-name.
   */
  def underlyingArgument(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}
    this.unseal.underlyingArgument.seal.asInstanceOf[Expr[T]]
  }
}

object Expr {

  import scala.internal.quoted._

  /** Converts a tuple `(T1, ..., Tn)` to `(Expr[T1], ..., Expr[Tn])` */
  type TupleOfExpr[Tup <: Tuple] = Tuple.Map[Tup, [X] =>> (given QuoteContext) => Expr[X]]

  /** `Expr.betaReduce(f)(x1, ..., xn)` is functionally the same as `'{($f)($x1, ..., $xn)}`, however it optimizes this call
   *   by returning the result of beta-reducing `f(x1, ..., xn)` if `f` is a known lambda expression.
   *
   *  `Expr.betaReduce` distributes applications of `Expr` over function arrows
   *   ```scala
   *   Expr.betaReduce(_): Expr[(T1, ..., Tn) => R] => ((Expr[T1], ..., Expr[Tn]) => Expr[R])
   *   ```
   */
  def betaReduce[F, Args <: Tuple, R, G](f: Expr[F])(given tf: TupledFunction[F, Args => R], tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]], qctx: QuoteContext): G = {
    import qctx.tasty.{_, given}
    tg.untupled(args => qctx.tasty.internal.betaReduce(f.unseal, args.toArray.toList.map(_.asInstanceOf[QuoteContext => Expr[_]](qctx).unseal)).seal.asInstanceOf[Expr[R]])
  }

  /** `Expr.betaReduceGiven(f)(x1, ..., xn)` is functionally the same as `'{($f)(given $x1, ..., $xn)}`, however it optimizes this call
   *   by returning the result of beta-reducing `f(given x1, ..., xn)` if `f` is a known lambda expression.
   *
   *  `Expr.betaReduceGiven` distributes applications of `Expr` over function arrows
   *   ```scala
   *   Expr.betaReduceGiven(_): Expr[(given T1, ..., Tn) => R] => ((Expr[T1], ..., Expr[Tn]) => Expr[R])
   *   ```
   */
  def betaReduceGiven[F, Args <: Tuple, R, G](f: Expr[F])(given tf: TupledFunction[F, (given Args) => R], tg: TupledFunction[G, TupleOfExpr[Args] => Expr[R]], qctx: QuoteContext): G = {
    import qctx.tasty.{_, given}
    tg.untupled(args => qctx.tasty.internal.betaReduce(f.unseal, args.toArray.toList.map(_.asInstanceOf[QuoteContext => Expr[_]](qctx).unseal)).seal.asInstanceOf[Expr[R]])
  }

  /** Returns a null expresssion equivalent to `'{null}` */
  def nullExpr: (given QuoteContext) => Expr[Null] = (given qctx) => {
    import qctx.tasty.{_, given}
    Literal(Constant(null)).seal.asInstanceOf[Expr[Null]]
  }

  /** Returns a unit expresssion equivalent to `'{}` or `'{()}` */
  def unitExpr: (given QuoteContext) => Expr[Unit] = (given qctx) => {
    import qctx.tasty.{_, given}
    Literal(Constant(())).seal.asInstanceOf[Expr[Unit]]
  }

  /** Returns an expression containing a block with the given statements and ending with the expresion
   *  Given list of statements `s1 :: s2 :: ... :: Nil` and an expression `e` the resulting expression
   *  will be equivalent to `'{ $s1; $s2; ...; $e }`.
   */
  def block[T](statements: List[Expr[_]], expr: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{_, given}
    Block(statements.map(_.unseal), expr.unseal).seal.asInstanceOf[Expr[T]]
  }

  /** Lift a value into an expression containing the construction of that value */
  def apply[T: Liftable](x: T)(given QuoteContext): Expr[T] = summon[Liftable[T]].toExpr(x)

  /** Lifts this sequence of expressions into an expression of a sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   *
   *  Usage:
   *  ```scala
   *  '{ List(${Expr.ofSeq(List(1, 2, 3))}: _*) } // equvalent to '{ List(1, 2, 3) }
   *  ```
   */
  def ofSeq[T](xs: Seq[Expr[T]])(given tp: Type[T], qctx: QuoteContext): Expr[Seq[T]] = {
    import qctx.tasty.{_, given}
    Repeated(xs.map(_.unseal).toList, tp.unseal).seal.asInstanceOf[Expr[Seq[T]]]
  }


  /** Lifts this list of expressions into an expression of a list
   *
   *  Transforms a list of expression
   *    `List(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ List($e1, $e2, ...) }` typed as an `Expr[List[T]]`
   */
  def  ofList[T](xs: Seq[Expr[T]])(given Type[T], QuoteContext): Expr[List[T]] =
    if (xs.isEmpty) '{ Nil } else '{ List(${ofSeq(xs)}: _*) }

  /** Lifts this sequence of expressions into an expression of a tuple
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[_]`
   *  to an expression equivalent to
   *    `'{ ($e1, $e2, ...) }` typed as an `Expr[Tuple]`
   */
  def ofTuple(seq: Seq[Expr[_]])(given QuoteContext): Expr[Tuple] = {
    seq match {
      case Seq() =>
        unitExpr
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
        '{ Tuple.fromIArray(IArray(${ofSeq(seq)}: _*)) }
    }
  }

  /** Given a tuple of the form `(Expr[A1], ..., Expr[An])`, outputs a tuple `Expr[(A1, ..., An)]`. */
  def ofTuple[T <: Tuple: Tuple.IsMappedBy[Expr]: Type](tup: T) (given qctx: QuoteContext): Expr[Tuple.InverseMap[T, Expr]] = {
    import qctx.tasty.{_, given}
    val elems: Seq[Expr[_]] = tup.asInstanceOf[Product].productIterator.toSeq.asInstanceOf[Seq[Expr[_]]]
    ofTuple(elems).cast[Tuple.InverseMap[T, Expr]]
  }

  // TODO generalize for any function arity (see Expr.betaReduce)
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(given qctx: QuoteContext): X = {
    import qctx.tasty.{given, _}
    val (params, bodyExpr) = paramsAndBody(f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](e.unseal, params, List(v.unseal)).seal.asInstanceOf[Expr[t]])
  }

  def open[T1, T2, R, X](f: Expr[(T1, T2) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2]) => Expr[t]) => X)(given qctx: QuoteContext)(given DummyImplicit): X = {
    import qctx.tasty.{given, _}
    val (params, bodyExpr) = paramsAndBody(f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2]) => bodyFn[t](e.unseal, params, List(v1.unseal, v2.unseal)).seal.asInstanceOf[Expr[t]])
  }

  def open[T1, T2, T3, R, X](f: Expr[(T1, T2, T3) => R])(content: (Expr[R], [t] => Expr[t] => (Expr[T1], Expr[T2], Expr[T3]) => Expr[t]) => X)(given qctx: QuoteContext)(given DummyImplicit, DummyImplicit): X = {
    import qctx.tasty.{given, _}
    val (params, bodyExpr) = paramsAndBody(f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v1: Expr[T1], v2: Expr[T2], v3: Expr[T3]) => bodyFn[t](e.unseal, params, List(v1.unseal, v2.unseal, v3.unseal)).seal.asInstanceOf[Expr[t]])
  }

  private def paramsAndBody[R](given qctx: QuoteContext)(f: Expr[Any]) = {
    import qctx.tasty.{given, _}
    val Block(List(DefDef("$anonfun", Nil, List(params), _, Some(body))), Closure(Ident("$anonfun"), None)) = f.unseal.etaExpand
    (params, body.seal.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](given qctx: QuoteContext)(e: qctx.tasty.Term, params: List[qctx.tasty.ValDef], args: List[qctx.tasty.Term]): qctx.tasty.Term = {
    import qctx.tasty.{given, _}
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(given ctx: Context): Term =
        super.transformTerm(tree) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)
  }

}
