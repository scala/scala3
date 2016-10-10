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

 /** Interpret one line of input. All feedback, including parse errors and
  *  evaluation results, are printed via the context's reporter. Values
  *  defined are available for future interpreted strings.
  */
  def interpret(line: String)(implicit ctx: Context): Result

  /** Tries to bind an id to a value, returns the outcome of trying to bind */
  def bind(id: String, boundType: String, value: AnyRef)(implicit ctx: Context): Result

  /** Suppress output during evaluation of `operation`. */
  def beQuietDuring[T](operation: => T): T

  /** Suppresses output and saves it for `lastOutput` to collect */
  def delayOutputDuring[T](operation: => T): T

  /** Gets the last output not printed immediately */
  def lastOutput(): Seq[String]
}
