package scala.quoted

import language.experimental.captureChecking

object Exprs:

  /** Matches literal sequence of literal constant value expressions and returns a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
   *    case Varargs(Exprs(args)) => ???
   *      // args: Seq[Int]
   *  ```
   *  To directly get the value of all expressions in a sequence `exprs: Seq[Expr[T]]` consider using `exprs.map(_.value)`/`exprs.map(_.valueOrError)` instead.
   *
   *  @tparam T the type of values being extracted from the expressions
   *  @param exprs the sequence of expressions to extract values from
   *  @param FromExpr the typeclass instance used to extract a value of type `T` from each `Expr[T]`
   *  @param Quotes the quotation context
   *  @return `Some` containing the sequence of extracted values if all expressions yield a value, or `None` if any expression cannot be converted
   */
  def unapply[T](exprs: Seq[Expr[T]])(using FromExpr[T])(using Quotes): Option[Seq[T]] =
    val builder = Seq.newBuilder[T]
    val iter = exprs.iterator
    while iter.hasNext do
      iter.next().value match
        case Some(value) => builder += value
        case _ => return None
    Some(builder.result())

end Exprs
