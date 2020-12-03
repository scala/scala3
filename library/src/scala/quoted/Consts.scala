package scala.quoted

/** Literal constant values */
object Consts {

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(usingusing Quotes): Expr[Int] = argsExpr match
   *    case Varargs(Consts(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   *
   *  To directly unlift all expressions in a sequence `exprs: Seq[Expr[T]]` consider using `exprs.map(_.value)`/`exprs.map(_.valueOrError)` insead.
   */
  def unapply[T](exprs: Seq[Expr[T]])(using Quotes): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Const(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
