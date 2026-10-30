package scala.util.control

import language.experimental.captureChecking

import scala.compiletime.uninitialized

/** Library implementation of nonlocal return.
 *
 *  Usage:
 *
 *     import scala.util.control.NonLocalReturns.*
 *
 *     returning { ... throwReturn(x) ... }
 *
 *  This API has been deprecated. Its functionality is better served by
 *
 *   - `scala.util.boundary` in place of `returning`
 *   - `scala.util.break` in place of `throwReturn`
 *
 *  The new abstractions work with plain `RuntimeExceptions` and are more
 *  performant, since returns within the scope of the same method can be
 *  rewritten by the compiler to jumps.
 */
@deprecated("Use scala.util.boundary instead", "3.3")
object NonLocalReturns {
  /** A throwable used to implement nonlocal returns.
   *
   *  @tparam T the type of the result to be returned
   */
  @deprecated("Use scala.util.boundary.Break instead", "3.3")
  class ReturnThrowable[T] extends ControlThrowable {
    private var myResult: T = uninitialized
    /** Throws this exception with the given result.
     *
     *  @param result the value to be returned
     */
    def throwReturn(result: T): Nothing = {
      myResult = result
      throw this
    }
    /** Returns the result stored in this exception. */
    def result: T = myResult
  }

  /** Performs a nonlocal return by throwing an exception. */
  @deprecated("Use scala.util.boundary.break instead", "3.3")
  def throwReturn[T](result: T)(using returner: ReturnThrowable[? >: T]): Nothing =
    returner.throwReturn(result)

  /** Enables nonlocal returns in `op`. */
  @deprecated("Use scala.util.boundary instead", "3.3")
  def returning[T](op: ReturnThrowable[T] ?=> T): T = {
    val returner = new ReturnThrowable[T]
    try op(using returner)
    catch {
      case ex: ReturnThrowable[T @unchecked] =>
       if (ex.eq(returner)) ex.result else throw ex
    }
  }
}
