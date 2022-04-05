package scala.quoted

/** Expression representation of literal sequence of expressions.
 *
 *  `Varargs` can be used to create the an expression `args` that will be used as varargs `'{ f($args: _*) }`
 *  or it can be used to extract all the arguments of the a varargs.
 */
object Varargs {

  /**
   *  Lifts this sequence of expressions into an expression of a sequence
   *
   *  Transforms a sequence of expression
   *    `Seq(e1, e2, ...)` where `ei: Expr[T]`
   *  to an expression equivalent to
   *    `'{ Seq($e1, $e2, ...) }` typed as an `Expr[Seq[T]]`
   *
   *  Usage:
   *  ```scala
   *  //{
   *  def f(using Quotes) = {
   *    import quotes.reflect.*
   *  //}
   *    '{ List(${Varargs(List('{1}, '{2}, '{3}))}: _*) } // equivalent to '{ List(1, 2, 3) }
   *  //{
   *  }
   *  //}
   *  ```
   */
  def apply[T](xs: Seq[Expr[T]])(using Type[T])(using Quotes): Expr[Seq[T]] = {
    import quotes.reflect._
    Repeated(xs.map(_.asTerm).toList, TypeTree.of[T]).asExpr.asInstanceOf[Expr[Seq[T]]]
  }

  /** Matches a literal sequence of expressions and return a sequence of expressions.
   *
   *  Usage:
   *  ```scala sc:macrocompile
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
   *    case Varargs(argVarargs) => ???
   *      // argVarargs: Seq[Expr[Int]]
   *
   */
  def unapply[T](expr: Expr[Seq[T]])(using Quotes): Option[Seq[Expr[T]]] = {
    import quotes.reflect._
    def rec(tree: Term): Option[Seq[Expr[T]]] = tree match {
      case Repeated(elems, _) => Some(elems.map(x => x.asExpr.asInstanceOf[Expr[T]]))
      case Typed(e, _) => rec(e)
      case Block(Nil, e) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.asTerm)
  }

}
