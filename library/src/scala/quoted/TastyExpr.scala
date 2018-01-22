package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

/** An Expr backed by a pickled TASTY tree */
final class TastyExpr[T](val tasty: Pickled, val args: Seq[Any]) extends Expr[T] with TastyQuoted {
  override def toString(): String = s"Expr(<pickled>)"
}
