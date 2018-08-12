package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[+T] {
  final def unary_~ : T = throw new Error("~ should have been compiled away")

  /** Evaluate the contents of this expression and return the result.
   *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  final def run(implicit toolbox: Toolbox): T = toolbox.run(this)

  /** Show a source code like representation of this expression */
  final def show(implicit toolbox: Toolbox): String = toolbox.show(this)
}

object Expr {
  /** A term quote is desugared by the compiler into a call to this method */
  def apply[T](x: T): Expr[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  implicit class AsFunction[T, U](private val f: Expr[T => U]) extends AnyVal {
    def apply(x: Expr[T]): Expr[U] = new Exprs.FunctionAppliedTo[T, U](f, x)
  }

}

/** All implementations of Expr[T].
 *  These should never be used directly.
 */
object Exprs {
  /** An Expr backed by a pickled TASTY tree */
  final class TastyExpr[+T](val tasty: Pickled, val args: Seq[Any]) extends Expr[T] {
    override def toString: String = s"Expr(<pickled tasty>)"
  }

  /** An Expr backed by a lifted value.
   *  Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.
   */
  final class LiftedExpr[+T](val value: T) extends Expr[T] {
    override def toString: String = s"Expr($value)"
  }

  /** An Expr backed by a tree. Only the current compiler trees are allowed.
   *
   *  These expressions are used for arguments of macros. They contain and actual tree
   *  from the program that is being expanded by the macro.
   *
   *  May contain references to code defined outside this TastyTreeExpr instance.
   */
  final class TastyTreeExpr[Tree](val tree: Tree) extends quoted.Expr[Any] {
    override def toString: String = s"Expr(<tasty tree>)"
  }

  /** An Expr representing `'{(~f).apply(~x)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[T, +U](val f: Expr[T => U], val x: Expr[T]) extends Expr[U] {
    override def toString: String = s"Expr($f <applied to> $x)"
  }
}
