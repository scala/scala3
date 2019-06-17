package scala

package quoted {

  sealed abstract class Expr[+T] {

    /** Evaluate the contents of this expression and return the result.
     *
     *  May throw a FreeVariableError on expressions that came from a macro.
     */
    @deprecated("Use scala.quoted.run", "")
    final def run(implicit toolbox: Toolbox): T = toolbox.run(this)

  }

}

package internal {
  package quoted {

    /** An Expr backed by a pickled TASTY tree */
    final class TastyExpr[+T](val tasty: scala.runtime.quoted.Unpickler.Pickled, val args: Seq[Any]) extends scala.quoted.Expr[T] {
      override def toString: String = s"Expr(<pickled tasty>)"
    }

    /** An Expr backed by a lifted value.
     *  Values can only be of type Boolean, Byte, Short, Char, Int, Long, Float, Double, Unit, String or Null.
     */
    final class LiftedExpr[+T](val value: T) extends scala.quoted.Expr[T] {
      override def toString: String = s"Expr($value)"
    }

    /** An Expr backed by a tree. Only the current compiler trees are allowed.
     *
     *  These expressions are used for arguments of macros. They contain and actual tree
     *  from the program that is being expanded by the macro.
     *
     *  May contain references to code defined outside this TastyTreeExpr instance.
     */
    final class TastyTreeExpr[Tree](val tree: Tree) extends scala.quoted.Expr[Any] {
      override def toString: String = s"Expr(<tasty tree>)"
    }

    // TODO Use a List in FunctionAppliedTo(val f: Expr[_], val args: List[Expr[_]])
    // FIXME: Having the List in the code above trigers an assertion error while testing dotty.tools.dotc.reporting.ErrorMessagesTests.i3187
    //        This test does redefine `scala.collection`. Further investigation is needed.
    /** An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known */
    final class FunctionAppliedTo[+R](val f: scala.quoted.Expr[_], val args: Array[scala.quoted.Expr[_]]) extends scala.quoted.Expr[R] {
      override def toString: String = s"Expr($f <applied to> ${args.toList})"
    }

  }
}
