package scala.quoted

/** MLiteral constant values */
object Const {

  /** Matches expressions containing literal constant values and extracts the value.
   *  It may match expressions of type Boolean, Byte, Short, Int, Long,
   *  Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
   *
   *  Usage:
   *  ```
   *  (x: Expr[B]) match {
   *    case Const(value: B) => ...
   *  }
   *  ```
   */
  def unapply[T](expr: Expr[T])(using qctx: QuoteContext): Option[T] = {
    import qctx.tasty.{_, given _}
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.unseal)
  }

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(usingusing QuoteContext): Expr[Int] = argsExpr match
   *    case Exprs(Const(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  def unapply[T](exprs: Seq[Expr[T]])(using qctx: QuoteContext): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Const(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }

}
