package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[+T] {

  /** Evaluate the contents of this expression and return the result.
   *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  final def run(implicit toolbox: Toolbox): T = toolbox.run(this)

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

  // TODO Use a List in FunctionAppliedTo(val f: Expr[_], val args: List[Expr[_]])
  // FIXME: Having the List in the code above trigers an assertion error while testing dotty.tools.dotc.reporting.ErrorMessagesTests.i3187
  //        This test does redefine `scala.collection`. Further investigation is needed.
  /** An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[+R](val f: Expr[_], val args: Array[Expr[_]]) extends Expr[R] {
    override def toString: String = s"Expr($f <applied to> ${args.toList})"
  }
}
