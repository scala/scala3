package scala.quoted

/** Literal sequence of expressions */
object Varargs {

  /** Lifts this sequence of expressions into an expression of a sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   *
   *  Usage:
   *  ```scala
   *  '{ List(${Varargs(List(1, 2, 3))}: _*) } // equvalent to '{ List(1, 2, 3) }
   *  ```
   */
  def apply[T](xs: Seq[Expr[T]])(using tp: Type[T], qctx: QuoteContext): Expr[Seq[T]] = {
    import qctx.reflect._
    Repeated(xs.map(Term.of).toList, TypeTree.of[T]).asExpr.asInstanceOf[Expr[Seq[T]]]
  }

  /** Matches a literal sequence of expressions and return a sequence of expressions.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using QuoteContext): Expr[Int] = argsExpr match
   *    case Varargs(argVarargs) =>
   *      // argVarargs: Seq[Expr[Int]]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](expr: Expr[Seq[T]])(using qctx: QuoteContext): Option[Seq[Expr[T]]] = {
    import qctx.reflect._
    def rec(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Typed(Repeated(elems, _), _) => Some(elems.map(x => x.asExpr.asInstanceOf[Expr[T]]))
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(Term.of(expr))
  }

}
