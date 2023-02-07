package concurrent

/** A trait for cancellable entiries that can be grouped */
trait Cancellable:

  def cancel(): Unit

  /** Add a given child to this Cancellable, so that the child will be cancelled
   *  when the Cancellable itself is cancelled.
   */
  def addChild(child: Cancellable): Unit

  def isCancelled: Boolean

end Cancellable
