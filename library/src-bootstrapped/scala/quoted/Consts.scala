package scala.quoted

/** Literal constant values */
object Consts {

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(using s: Scope)(argsExpr: s.Expr[Seq[Int]]): s.Expr[Int] = argsExpr match
   *    case Varargs(Consts(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](using s: Scope)(exprs: Seq[s.Expr[T]]): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Const(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
