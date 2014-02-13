package dotty

package object tools {
  // TODO(lrytz) there also exists the class `dotty.tools.dotc.FatalError`. The aliases here
  // seem to be used in the `dotty.tools.io` package. Needs clean up. If `io` needs a different
  // exception, it should have a different name.
  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  val ListOfNil = Nil :: Nil

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
}
