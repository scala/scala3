package dotty.tools.dotc.transform.linker

import scala.collection.{immutable, mutable}

final class WorkList[A] {
  private val currentReachableItems = mutable.Set[A]()
  private var newReachableItems = immutable.Set[A]()

  def +=(elem: A): Unit = {
    // No new elements are accepted if they've already been reachable before
    if (!currentReachableItems(elem)) {
      newReachableItems += elem
      currentReachableItems += elem
    }
  }

  /**
    * Add new items to the work list. It also adds the items to the reachable set.
    */
  def ++=(xs: TraversableOnce[A]): this.type = { xs.seq foreach +=; this }

  /** Clear the new items */
  def clear(): Unit = {
    newReachableItems = immutable.Set[A]()
  }

  /** Do we have new items to process? */
  def nonEmpty: Boolean = newReachableItems.nonEmpty

  /** How many new items do we have? */
  def size: Int = newReachableItems.size

  def newItems: Set[A] = newReachableItems

  def reachableItems: Set[A] = currentReachableItems.toSet

  def contains(x: A): Boolean = currentReachableItems.contains(x)
}
