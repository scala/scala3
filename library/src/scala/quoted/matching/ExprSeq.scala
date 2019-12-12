package scala.quoted
package matching

/** Literal sequence of expressions */
object ExprSeq {

  /** Matches a literal sequence of expressions and return a sequence of expressions.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(given QuoteContext): Expr[Int] = argsExpr match
   *    case ExprSeq(argExprs) =>
   *      // argExprs: Seq[Expr[Int]]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](expr: Expr[Seq[T]])(given qctx: QuoteContext): Option[Seq[Expr[T]]] = {
    import qctx.tasty.{_, given}
    def rec(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Typed(Repeated(elems, _), _) => Some(elems.map(x => x.seal.asInstanceOf[Expr[T]]))
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

}
