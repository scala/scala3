package scala.quoted

/** Value expressions */
object Unlifted {

  /** Matches expressions containing values and extracts the value.
   *
   *  Usage:
   *  ```
   *  case '{ ... ${expr @ Unlifted(value)}: T ...} =>
   *    // expr: Expr[T]
   *    // value: T
   *  ```
   *
   *  To directly unlift an expression `expr: Expr[T]` consider using `expr.unlift`/`expr.unliftOrError` insead.
   */
  def unapply[T](expr: Expr[T])(using Unliftable[T])(using Quotes): Option[T] =
    summon[Unliftable[T]].fromExpr(expr)

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
   *    case Varargs(Unlifted(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   *  To directly unlift all expressions in a sequence `exprs: Seq[Expr[T]]` consider using `exprs.map(_.unlift)`/`exprs.map(_.unliftOrError)` insead.
   */
  def unapply[T](exprs: Seq[Expr[T]])(using Unliftable[T])(using Quotes): Option[Seq[T]] =
    val builder = Seq.newBuilder[T]
    val iter = exprs.iterator
    while iter.hasNext do
      iter.next() match
        case Unlifted(value) => builder += value
        case _ => return None
    Some(builder.result())

}
