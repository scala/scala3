package scala.quoted
package matching

/** Value sequence of value expressions */
object ValueSeq {

  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using QuoteContext): Expr[Int] = argsExpr match
   *    case ValueSeq(args) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   */
  @deprecated("use scala.quoted.Varargs(scala.quoted.Value(_)) instead", "0.23.0")
  def unapply[T](expr: Expr[Seq[T]])(using unlift: Unliftable[T], qctx: QuoteContext): Option[Seq[T]] =
    import scala.quoted.Const
    expr match
      case Varargs(Values(elems)) => Some(elems)
      case _ => None

}
