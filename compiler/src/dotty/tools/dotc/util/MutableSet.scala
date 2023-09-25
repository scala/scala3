package dotty.tools.dotc.util

/** A common class for lightweight mutable sets.
 */
abstract class MutableSet[T] extends ReadOnlySet[T]:

  /** Add element `x` to the set */
  def +=(x: T): Unit

  /** attempts to put `x` in the Set, if it was not entered before, return true, else return false.
   *  Overridden in GenericHashSet.
   */
  def add(x: T): Boolean =
    if lookup(x) == null then { this += x; true }
    else false

  /** Like `+=` but return existing element equal to `x` of it exists,
   *  `x` itself otherwise.
   */
  def put(x: T): T

  /** Remove element `x` from the set */
  def -=(x: T): Unit

  /** Remove all elements from this set.
   *  @param resetToInitial If true, set back to initial configuration, which includes
   *                        reallocating tables.
   */
  def clear(resetToInitial: Boolean = true): Unit

  def ++= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this += _)

  def --= (xs: IterableOnce[T]): Unit =
    xs.iterator.foreach(this -= _)

