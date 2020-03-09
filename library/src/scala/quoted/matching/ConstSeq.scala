package scala.quoted
package matching

/** Literal sequence of literal constant value expressions */
object ConstSeq {

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(usingusing QuoteContext): Expr[Int] = argsExpr match
   *    case ConstSeq(args) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  @deprecated("use scala.quoted.Varargs(scala.quoted.Const(_)) instead", "0.23.0")
  def unapply[T](expr: Expr[Seq[T]])(using qctx: QuoteContext): Option[Seq[T]] =
    import scala.quoted.Const
    expr match
      case Varargs(Consts(elems)) => Some(elems)
      case _ => None

}
