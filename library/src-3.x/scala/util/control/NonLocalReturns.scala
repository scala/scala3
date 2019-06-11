package scala.util.control

/** Library implementation of nonlocal return.
 *
 *  Usage:
 *
 *     import scala.util.control.NonLocalReturns._
 *
 *     returning { ... throwReturn(x) ... }
 */
object NonLocalReturns {
  class ReturnThrowable[T] extends ControlThrowable {
    private var myResult: T = _
    def throwReturn(result: T): Nothing = {
      myResult = result
      throw this
    }
    def result: T = myResult
  }

  /** Performs a nonlocal return by throwing an exception. */
  def throwReturn[T](result: T) given (returner: ReturnThrowable[T]): Nothing =
    returner.throwReturn(result)

  /** Enable nonlocal returns in `op`. */
  def returning[T](op: given ReturnThrowable[T] => T): T = {
    val returner = new ReturnThrowable[T]
    try op given returner
    catch {
      case ex: ReturnThrowable[T] =>
       if (ex.eq(returner)) ex.result else throw ex
    }
  }
}
