package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

/** An Expr backed by a pickled TASTY tree */
final case class TastyExpr[T](tasty: Pickled, args: Seq[Any]) extends Expr[T] with TastyQuoted
