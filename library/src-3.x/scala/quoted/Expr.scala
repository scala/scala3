package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

sealed abstract class Expr[+T] {

  /** Evaluate the contents of this expression and return the result.
   *
   *  May throw a FreeVariableError on expressions that came from a macro.
   */
  final def run(implicit toolbox: Toolbox): T = toolbox.run(this)

}

object Expr {

  implicit class ExprOps[T](expr: Expr[T]) {
    /** Show a source code like representation of this expression */
    def show(implicit toolbox: Toolbox): String = toolbox.show(expr)
  }

  implicit object FunctionBetaReduction {

    /** Beta-reduces the function appication. Generates the an expression only containing the body of the function */
    def (f: Expr[() => R]) apply[R] (): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array.empty)

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1) => R]) apply[T1, R](x1: Expr[T1]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2) => R]) apply[T1, T2, R](x1: Expr[T1], x2: Expr[T2]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3) => R]) apply[T1, T2, T3, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4) => R]) apply[T1, T2, T3, T4, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5) => R]) apply[T1, T2, T3, T4, T5, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6) => R]) apply[T1, T2, T3, T4, T5, T6, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7) => R]) apply[T1, T2, T3, T4, T5, T6, T7, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))

    /** Beta-reduces the function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21], x22: Expr[T22]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))

  }

  implicit object ContextualFunctionBetaReduction {

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1) => R]) apply[T1, R](x1: Expr[T1]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2) => R]) apply[T1, T2, R](x1: Expr[T1], x2: Expr[T2]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3) => R]) apply[T1, T2, T3, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4) => R]) apply[T1, T2, T3, T4, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5) => R]) apply[T1, T2, T3, T4, T5, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6) => R]) apply[T1, T2, T3, T4, T5, T6, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7) => R]) apply[T1, T2, T3, T4, T5, T6, T7, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))

    /** Beta-reduces the contextual function appication.
     *  Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments
     */
    def (f: Expr[given (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R]) apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](x1: Expr[T1], x2: Expr[T2], x3: Expr[T3], x4: Expr[T4], x5: Expr[T5], x6: Expr[T6], x7: Expr[T7], x8: Expr[T8], x9: Expr[T9], x10: Expr[T10], x11: Expr[T11], x12: Expr[T12], x13: Expr[T13], x14: Expr[T14], x15: Expr[T15], x16: Expr[T16], x17: Expr[T17], x18: Expr[T18], x19: Expr[T19], x20: Expr[T20], x21: Expr[T21], x22: Expr[T22]): Expr[R] =
      new Exprs.FunctionAppliedTo[R](f, Array(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))

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
