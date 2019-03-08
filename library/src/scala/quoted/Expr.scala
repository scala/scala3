package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[+T] {

  /** Evaluate the contents of this expression and return the result.
   *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  final def run(implicit toolbox: Toolbox): T = toolbox.run(this)

  /** Show a source code like representation of this expression */
  final def show(implicit toolbox: Toolbox): String = toolbox.show(this)

}

object Expr {

  // TODO simplify using new extension methods

  implicit class AsFunction0[R](private val f: Expr[() => R]) extends AnyVal {
    def apply(): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array.empty)
  }

  implicit class AsFunction1[T1, R](private val f: Expr[(T1) => R]) extends AnyVal {
    def apply(x1: Expr[T1]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1))
  }

  implicit class AsFunction2[T1, T2, R](private val f: Expr[(T1, T2) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2))
  }

  implicit class AsFunction3[T1, T2, T3, R](private val f: Expr[(T1, T2, T3) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3))
  }

  implicit class AsFunction4[T1, T2, T3, T4, R](private val f: Expr[(T1, T2, T3, T4) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4))
  }

  implicit class AsFunction5[T1, T2, T3, T4, T5, R](private val f: Expr[(T1, T2, T3, T4, T5) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5))
  }

  implicit class AsFunction6[T1, T2, T3, T4, T5, T6, R](private val f: Expr[(T1, T2, T3, T4, T5, T6) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6))
  }

  implicit class AsFunction7[T1, T2, T3, T4, T5, T6, T7, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7))
  }

  implicit class AsFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8))
  }

  implicit class AsFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9))
  }

  implicit class AsFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
  }

  implicit class AsFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
  }

  implicit class AsFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))
  }

  implicit class AsFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))
  }

  implicit class AsFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
  }

  implicit class AsFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))
  }

  implicit class AsFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))
  }

  implicit class AsFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))
  }

  implicit class AsFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))
  }

  implicit class AsFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))
  }

  implicit class AsFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))
  }

  implicit class AsFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))
  }

  implicit class AsFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](private val f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R]) extends AnyVal {
    def apply(x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21], x22: Expr[T22]): Expr[R] = new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))
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

  // TODO Use a List in FunctionAppliedTo(val f: Expr[_], val args: List[Expr[_]])
  // FIXME: Having the List in the code above trigers an assertion error while testing dotty.tools.dotc.reporting.ErrorMessagesTests.i3187
  //        This test does redefine `scala.collection`. Further investigation is needed.
  /** An Expr representing `'{($f).apply($x1, ..., $xn)}` but it is beta-reduced when the closure is known */
  final class FunctionAppliedTo[+R](val f: Expr[_], val args: Array[Expr[_]]) extends Expr[R] {
    override def toString: String = s"Expr($f <applied to> ${args.toList})"
  }
}
