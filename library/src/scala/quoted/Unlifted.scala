package scala.quoted

/** Value expressions */
object Unlifted {

  /** Matches expressions containing values and extracts the value.
   *
   *  Usage:
   *  ```
   *  (x: Expr[B]) match {
   *    case Unlifted(value) => ... // value: B
   *  }
   *  ```
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
