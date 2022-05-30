package scala.util.control

/** Library implementation of nonlocal return.
 *
 *  Usage:
 *
 *     import scala.util.control.NonLocalReturns.*
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
  def throwReturn[T](result: T)(using returner: ReturnThrowable[T]): Nothing =
    returner.throwReturn(result)

  /** Enable nonlocal returns in `op`. */
  def returning[T](op: ReturnThrowable[T] ?=> T): T = {
    val returner = new ReturnThrowable[T]
    try op(using returner)
    catch {
      case ex: ReturnThrowable[T] =>
       if (ex.eq(returner)) ex.result else throw ex
    }
  }
}
