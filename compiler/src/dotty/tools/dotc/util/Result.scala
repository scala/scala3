package dotty.tools.dotc.util

/** Class optimized unapply result that does not wrap.
 *
 *  WARNING: Do not use Result[Result[T]], use Result[Option[T]] instead.
 *
 *  This is a simplification of general unboxed Option.
 *
 *  @param result result of the extractor or Result.Empty
 *  @tparam T type of the result (must not be a Result)
 */
class Result[T] private (val result: Any) extends AnyVal {

  def isEmpty: Boolean = result == Result.Empty

  def get: T = result.asInstanceOf[T]

  def isDefined: Boolean = !isEmpty
}

object Result {
  // TODO optimize further and assume the result cannot be null?
  private[Result] object Empty

  def apply[T](x: T): Result[T] = new Result[T](x)

  def empty[T]: Result[T] = new Result[T](Empty)

  def from[T](option: Option[T]): Result[T] =
    if (option.isEmpty) empty else apply(option.get)
}
