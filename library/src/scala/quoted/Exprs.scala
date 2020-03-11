package scala.quoted

/** Sequences of expressions */
object Exprs {

  /** Lift a sequence of values into a sequense of expressions containing the construction of each value */
  def apply[T](xs: Seq[T])(using lift: Liftable[T], qctx: QuoteContext): Seq[Expr[T]] =
    xs.map(x => Expr(x))

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using QuoteContext): Expr[Int] = argsExpr match
   *    case Varargs(Exprs(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](exprs: Seq[Expr[T]])(using valueOf: ValueOfExpr[T], qctx: QuoteContext): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Expr(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
