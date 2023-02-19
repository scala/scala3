package concurrent

/** A trait for cancellable entities that can be grouped */
trait Cancellable:

  /** Issue a cancel request */
  def cancel(): Unit

  /** Add a given child to this Cancellable, so that the child will be cancelled
   *  when the Cancellable itself is cancelled.
   */
  def addChild(child: Cancellable): Unit

object Cancellable:

  /** A cancelled entity that ignores all `cancel` and `addChild` requests */
  object empty extends Cancellable:
    def cancel() = ()
    def addChild(child: Cancellable) = ()

end Cancellable
