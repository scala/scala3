package scala.quoted
package matching

/** Literal sequence of literal constant value expressions */
object ConstSeq {

  /** Matches literal sequence of literal constant value expressions */
  def unapply[T](expr: Expr[Seq[T]]) given (qctx: QuoteContext): Option[Seq[T]] = expr match {
    case ExprSeq(elems) =>
      elems.foldRight(Option(List.empty[T])) { (elem, acc) =>
        (elem, acc) match {
          case (Const(value), Some(lst)) => Some(value :: lst)
          case (_, _) => None
        }
      }
    case _ => None
  }

}
