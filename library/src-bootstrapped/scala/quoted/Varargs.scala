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
  def apply[T](using s: Scope)(xs: Seq[s.Expr[T]])(using tp: s.Type[T]): s.Expr[Seq[T]] = {
    import s.tasty._
    Repeated(xs.toList, tp).seal.asInstanceOf[s.Expr[Seq[T]]]
  }

  /** Matches a literal sequence of expressions and return a sequence of expressions.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(using s: Scope)(argsExpr: s.Expr[Seq[Int]]): s.Expr[Int] = argsExpr match
   *    case Varargs(argVarargs) =>
   *      // argVarargs: Seq[s.Expr[Int]]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](using s: Scope)(expr: s.Expr[Seq[T]]): Option[Seq[s.Expr[T]]] = {
    import s.tasty._
    def rec(tree: Term): Option[Seq[s.Expr[T]]] = tree match {
      case Typed(Repeated(elems, _), _) => Some(elems.map(x => x.seal.asInstanceOf[s.Expr[T]]))
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr)
  }

}
