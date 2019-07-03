package dotty

package object tools {
  // Ensure this object is already classloaded, since it's only actually used
  // when handling stack overflows and every operation (including class loading)
  // risks failing.
  dotty.tools.dotc.core.handleRecursive

  val ListOfNil: List[Nil.type] = Nil :: Nil

  /** True if two lists have the same length.  Since calling length on linear sequences
   *  is O(n), it is an inadvisable way to test length equality.
   */
  final def sameLength[T](xs: List[T], ys: List[T]): Boolean = xs match {
    case _ :: xs1 =>
      ys match {
        case _ :: ys1 => sameLength(xs1, ys1)
        case _ => false
      }
    case _ => ys.isEmpty
  }

  /** Throws an `UnsupportedOperationException` with the given method name. */
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(methodName)

  object WrappedResult {
    opaque type Type[T] = T
    def unwrap[T](x: Type[T]): T = x
    def apply[T](x: T): Type[T] = x
  }
  type WrappedResult[T] = WrappedResult.Type[T]
  def result[T] given (x: WrappedResult[T]): T = WrappedResult.unwrap(x)
}
