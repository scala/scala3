package scala.quoted.matching

import scala.quoted.Expr

import scala.tasty.Reflection // TODO do not depend on reflection directly

/** Literal sequence of literal constant value expressions */
object ConstSeq {

  /** Matches literal sequence of literal constant value expressions */
  def unapply[T](expr: Expr[Seq[T]]) given Reflection: Option[Seq[T]] = expr match {
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
