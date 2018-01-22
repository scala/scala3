package scala.quoted

import scala.runtime.quoted.Runner
import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[T] extends Quoted {
  final def unary_~ : T = throw new Error("~ should have been compiled away")
  final def run(implicit runner: Runner[T]): T = runner.run(this)
  final def show(implicit runner: Runner[T]): String = runner.show(this)
}

object Expr {
  implicit def toExpr[T](x: T)(implicit ev: Liftable[T]): Expr[T] =
    ev.toExpr(x)

  implicit class AsFunction[T, U](private val f: Expr[T => U]) extends AnyVal {
    def apply(x: Expr[T]): Expr[U] = new Exprs.FunctionAppliedTo[T, U](f, x)
  }

}

/** All implementations of Expr[T] */
object Exprs {
  /** An Expr backed by a pickled TASTY tree */
  final class TastyExpr[T](val tasty: Pickled, val args: Seq[Any]) extends Expr[T] {
    override def toString(): String = s"Expr(<pickled>)"
  }

  /** An Expr backed by a value */
  final class ConstantExpr[T](val value: T) extends Expr[T] {
    override def toString: String = s"Expr($value)"
  }

  /** An Expr backed by a tree */
  final class RawExpr[Tree](val tree: Tree) extends quoted.Expr[Any] {
    override def toString: String = s"Expr(<raw>)"
  }

  /** An Expr representing `'{(~f).apply(~x)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[T, U](val f: Expr[T => U], val x: Expr[T]) extends Expr[U] {
    override def toString: String = s"Expr($f <applied to> $x)"
  }
}
