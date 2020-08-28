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
  def unapply[T](expr: Expr[T])(using unlift: Unliftable[T], qxtc: QuoteContext): Option[T] =
    unlift.fromExpr(expr)

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using QuoteContext): Expr[Int] = argsExpr match
   *    case Varargs(Unlifted(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](exprs: Seq[Expr[T]])(using unlift: Unliftable[T], qctx: QuoteContext): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Unlifted(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
