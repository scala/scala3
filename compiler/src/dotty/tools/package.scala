package dotty

package object tools {

  /** Cached single-element list of Nil. (Whether this helps performance has not been tested) */
  val ListOfNil: List[Nil.type] = Nil :: Nil

  /** Throws an `UnsupportedOperationException` with the given method name. */
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(methodName)

  /** Throws a `MatchError` with the given argument, or a default "unreachable" message. */
  def unreachable(x: Any = "<< this case was declared unreachable >>"): Nothing =
    throw new MatchError(x)

  /** Forward-ported from the explicit-nulls branch. */
  extension [T](x: T | Null)
    /** Should be used when we know from the context that `x` is not null.
     *  Flow-typing under explicit nulls will automatically insert many necessary
     *  occurrences of uncheckedNN.
     */
    transparent inline def uncheckedNN: T = x.asInstanceOf[T]
  end extension

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

  // Ensure this object is already classloaded, since it's only actually used
  // when handling stack overflows and every operation (including class loading)
  // risks failing.
  dotty.tools.dotc.core.handleRecursive
 }
