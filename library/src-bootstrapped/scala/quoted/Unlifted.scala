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
  def unapply[T](using s: Scope)(expr: s.Expr[T])(using unlift: s.Unliftable[T]): Option[T] =
    unlift.fromExpr(expr)

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(using s: Scope)(argsExpr: s.Expr[Seq[Int]]): s.Expr[Int] = argsExpr match
   *    case Varargs(Unlifted(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](using s: Scope)(exprs: Seq[s.Expr[T]])(using unlift: s.Unliftable[T]): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Unlifted(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
