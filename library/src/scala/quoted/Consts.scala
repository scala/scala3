package scala.quoted

/** Literal constant values */
@deprecated("Use `scala.quoted.Exprs` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
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
  @deprecated("Use `scala.quoted.Exprs.unapply` instead. This will be removed in 3.0.0-RC1", "3.0.0-M3")
  def unapply[T](exprs: Seq[Expr[T]])(using Quotes): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Const(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
