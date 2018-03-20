package scala.quoted

import scala.runtime.quoted.Toolbox
import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[T] {
  final def unary_~ : T = throw new Error("~ should have been compiled away")
  final def run(implicit toolbox: Toolbox[T]): T = toolbox.run(this)
  final def show(implicit toolbox: Toolbox[T]): String = toolbox.show(this)
}

object Expr {

  implicit class AsFunction[T, U](private val f: Expr[T => U]) extends AnyVal {
    def apply(x: Expr[T]): Expr[U] = new Exprs.FunctionAppliedTo[T, U](f, x)
  }

}

/** All implementations of Expr[T].
 *  These should never be used directly.
 */
object Exprs {
  /** An Expr backed by a pickled TASTY tree */
  final class TastyExpr[T](val tasty: Pickled, val args: Seq[Any]) extends Expr[T] {
    override def toString: String = s"Expr(<pickled>)"
  }

  /** An Expr backed by a lifted value.
   *  Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.
   */
  final class LiftedExpr[T](val value: T) extends Expr[T] {
    override def toString: String = s"Expr($value)"
  }

  /** An Expr backed by a tree. Only the current compiler trees are allowed. */
  final class TreeExpr[Tree](val tree: Tree) extends quoted.Expr[Any] {
    override def toString: String = s"Expr(<raw>)"
  }

  /** An Expr representing `'{(~f).apply(~x)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[T, U](val f: Expr[T => U], val x: Expr[T]) extends Expr[U] {
    override def toString: String = s"Expr($f <applied to> $x)"
  }
}
