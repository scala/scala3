package dotty

import scala.annotation.nowarn

package object tools {

  /** Cached single-element list of Nil. (Whether this helps performance has not been tested) */
  val ListOfNil: List[Nil.type] = Nil :: Nil

  /** Throws an `UnsupportedOperationException` with the given method name. */
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(methodName)

  /** Throws a `MatchError` with the given argument, or a default "unreachable" message. */
  def unreachable(x: Any = "<< this case was declared unreachable >>"): Nothing =
    throw new MatchError(x)

  /**
   * Allows one to lazily initialize values without explicit `.nn`.
   * This is useful for values that need a Context and thus can't be `lazy val`s.
   */
  inline def initialize[T](getter: T | Null, inline setter: T => Unit, inline value: => T): T =
    if getter != null then
      getter
    else
      val res = value
      setter(res)
      res

  /**
   * Prints the given text if the given operation throws an AssertionError, then rethrows.
   */
  @nowarn("msg=Catching AssertionError can lead to unexpected behavior") // we immediately rethrow
  inline def printOnAssertionError[T](text: => String)(inline op: => T): T =
    try
      op
    catch
      case ex: AssertionError =>
        println(text)
        throw ex

  /**
   * Infrastructure to shorten method calls by not requiring a lambda.
   * Instead of `def f(x: ... => ...)` that must be called as, e.g.,  `f(x => x + 1)`,
   * write `def f(x: WrappedResult[...] ?=> ...)`, use that parameter by creating a `WrappedResult`,
   * and call `f(result + 1)`.
   */
  private object resultWrapper {
    opaque type WrappedResult[T] = T
    private[tools] def unwrap[T](x: WrappedResult[T]): T = x
    private[tools] def wrap[T](x: T): WrappedResult[T] = x
  }
  type WrappedResult[T] = resultWrapper.WrappedResult[T]
  def WrappedResult[T](x: T): WrappedResult[T] = resultWrapper.wrap(x)
  def result[T](using x: WrappedResult[T]): T = resultWrapper.unwrap(x)
 }
