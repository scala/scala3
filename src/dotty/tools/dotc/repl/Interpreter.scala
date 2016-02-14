package dotty.tools
package dotc
package repl

import core.Contexts.Context

/** This object defines the type of interpreter results */
object Interpreter {

  /** A result from interpreting one line of input. */
  abstract sealed class Result

  /** The line was interpreted successfully. */
  case object Success extends Result

  /** The line was erroneous in some way. */
  case object Error extends Result

  /** The input was incomplete.  The caller should request more input.
   */
  case object Incomplete extends Result
}

/** The exported functionality of the interpreter */
trait Interpreter {
  import Interpreter._

  /** Interpret one line of input.  All feedback, including parse errors
   *  and evaluation results, are printed via the context's reporter.
   *  reporter.  Values defined are available for future interpreted strings.
   */
  def interpret(line: String)(implicit ctx: Context): Result

  /** Compile a string without exectuting the result.
   *  Returns true if there are no compilation errors, false otherwise.
   */
  def compileString(code: String)(implicit ctx: Context): Boolean

  /** Suppress output during evaluation of `operation`. */
  def beQuietDuring[T](operation: => T): T

  /** The interpreter settings */
  def isettings: InterpreterSettings

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret. Can be used to
   *  programmatically change intepreter settings.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any)(implicit ctx: Context): Result
}
