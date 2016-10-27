package dotty.tools.dotc.transform.linker

import scala.collection.{immutable, mutable}

class WorkList[A] {
  private val currentReachableItems = mutable.Set[A]()
  private val newReachableItems = mutable.Set[A]()

  def +=(elem: A): Unit = {
    // No new elements are accepted if they've already been reachable before
    if (!currentReachableItems(elem)) {
      newReachableItems += elem
      currentReachableItems += elem
    }
  }

  /** Add new items to the work list. It also adds the items to the reachable set. */
  def ++=(xs: TraversableOnce[A]): this.type = { xs.seq foreach +=; this }

  /** Clear the new items */
  def clearNewItems(): Unit = {
    newReachableItems.clear()
  }

  /** Returns true iff newItems would not return an empty set. */
  def hasNewItems: Boolean = newReachableItems.nonEmpty

  /** Return the set of new items added since last call to clearNewItems
    * or reachableItems if clearNewItems has never been called.
    */
  def newItems: immutable.Set[A] = newReachableItems.toSet

  /** Return the set of new items added to the work list */
  def items: immutable.Set[A] = currentReachableItems.toSet

  /** Returns true if the work list contains the element */
  def contains(x: A): Boolean = currentReachableItems.contains(x)
}
