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

  /** Forward-ported from the explicit-nulls branch. */
  extension [T](x: T | Null)
    /** Should be used when we know from the context that `x` is not null.
     *  Flow-typing under explicit nulls will automatically insert many necessary
     *  occurrences of uncheckedNN.
     */
    inline def uncheckedNN: T = x.asInstanceOf[T]

    inline def toOption: Option[T] =
      if x == null then None else Some(x.asInstanceOf[T])
  end extension

  /** Nullable eq and ne. */
  extension [T <: AnyRef](x: T | Null)
    inline def eqn (y: T | Null) =
      x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]

    inline def nen(y: T | Null): Boolean = !eqn(y)

  object resultWrapper {
    opaque type WrappedResult[T] = T
    private[tools] def unwrap[T](x: WrappedResult[T]): T = x
    private[tools] def wrap[T](x: T): WrappedResult[T] = x
  }
  type WrappedResult[T] = resultWrapper.WrappedResult[T]
  def WrappedResult[T](x: T) = resultWrapper.wrap(x)
  def result[T](using x: WrappedResult[T]): T = resultWrapper.unwrap(x)

  def unreachable(x: Any = "<< this case was declared unreachable >>"): Nothing =
    throw new MatchError(x)
}
