package scala.quoted

import language.experimental.captureChecking

/** Expression representation of literal sequence of expressions.
 *
 *  `Varargs` can be used to create the an expression `args` that will be used as varargs `'{ f($args: _*) }`
 *  or it can be used to extract all the arguments of the a varargs.
 */
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
   *  //{
   *  def f(using Quotes) = {
   *    import quotes.reflect.*
   *  //}
   *    '{ List(${Varargs(List('{1}, '{2}, '{3}))}: _*) } // equivalent to '{ List(1, 2, 3) }
   *  //{
   *  }
   *  //}
   *  ```
   *
   *  @tparam T the element type of the expressions in the sequence
   *  @param xs the sequence of individual expressions to lift into a single expression
   *  @return an expression representing the sequence `Seq(xs(0), xs(1), ...)` as an `Expr[Seq[T]]`
   */
  def apply[T](xs: Seq[Expr[T]])(using Type[T])(using Quotes): Expr[Seq[T]] = {
    import quotes.reflect.*
    Repeated(xs.map(_.asTerm).toList, TypeTree.of[T]).asExpr.asInstanceOf[Expr[Seq[T]]]
  }

  /** Matches a literal sequence of expressions and returns a sequence of expressions.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
   *    case Varargs(argVarargs) => ???
   *      // argVarargs: Seq[Expr[Int]]
   *  ```
   *
   *  @tparam T the element type of the varargs sequence
   *  @param expr the expression of a sequence to destructure into individual expressions
   *  @return `Some` containing the individual element expressions, or `None` if the expression is not a literal sequence
   */
  def unapply[T](expr: Expr[Seq[T]])(using Quotes): Option[Seq[Expr[T]]] = {
    import quotes.reflect.*
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
