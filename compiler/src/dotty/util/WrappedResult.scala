package dotty.util

object WrappedResult {
  opaque type WrappedResult[T] = T
  def result[T] given (x: WrappedResult[T]): T = x
  def apply[T](x: T): WrappedResult[T] = x
}
