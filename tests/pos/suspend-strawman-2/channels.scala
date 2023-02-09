package concurrent
import scala.collection.mutable, mutable.ListBuffer
import scala.util.boundary.Label
import runtime.suspend
import Async.{Listener, await}

/** An unbounded channel */
class UnboundedChannel[T] extends Async.ComposableSource[T]:
  private val pending = ListBuffer[T]()
  private val waiting = mutable.Set[Listener[T]]()

  private def drainWaiting(x: T): Boolean =
    val it = waiting.iterator
    var sent = false
    while it.hasNext && !sent do
      val k = it.next()
      sent = k(x)
      if sent then waiting -= k
    sent

  private def drainPending(k: Listener[T]): Boolean =
    val sent = pending.nonEmpty && k(pending.head)
    if sent then
      while
        pending.dropInPlace(1)
        pending.nonEmpty && drainWaiting(pending.head)
      do ()
    sent

  def read()(using Async): T = synchronized:
    await(this)

  def send(x: T): Unit = synchronized:
    val sent = pending.isEmpty && drainWaiting(x)
    if !sent then pending += x

  def poll(k: Listener[T]): Boolean = synchronized:
    drainPending(k)

  def onComplete(k: Listener[T]): Unit = synchronized:
    if !drainPending(k) then waiting += k

  def dropListener(k: Listener[T]): Unit = synchronized:
    waiting -= k

end UnboundedChannel

trait SyncChannel[T]:
  def canRead: Async.Source[T]
  def canSend: Async.Source[Listener[T]]

  def send(x: T)(using Async): Unit = await(canSend)(x)

  def read()(using Async): T = await(canRead)

object SyncChannel:
  def apply[T](): SyncChannel[T] = new Impl[T]:
    val canRead = new ReadSource
    val canSend = new SendSource

  abstract class Impl[T] extends SyncChannel[T]:
    protected val pendingReads = mutable.Set[Listener[T]]()
    protected val pendingSends = mutable.Set[Listener[Listener[T]]]()

    protected def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      pending.headOption match
        case Some(elem) => op(elem); true
        case None => false

    private def collapse[T](k2: Listener[Listener[T]]): Option[T] =
      var r: Option[T] = None
      if k2 { x => r = Some(x); true } then r else None

    protected class ReadSource extends Async.Source[T]:
      def poll(k: Listener[T]): Boolean =
        link(pendingSends, sender => collapse(sender).map(k) == Some(true))
      def onComplete(k: Listener[T]): Unit =
        if !poll(k) then pendingReads += k
      def dropListener(k: Listener[T]): Unit =
        pendingReads -= k

    protected class SendSource extends Async.Source[Listener[T]]:
      def poll(k: Listener[Listener[T]]): Boolean =
        link(pendingReads, k(_))
      def onComplete(k: Listener[Listener[T]]): Unit =
        if !poll(k) then pendingSends += k
      def dropListener(k: Listener[Listener[T]]): Unit =
        pendingSends -= k
  end Impl

end SyncChannel

trait ComposableSyncChannel[T] extends SyncChannel[T]:
  def canRead: Async.ComposableSource[T]
  def canSend: Async.ComposableSource[Listener[T]]

object ComposableSyncChannel:
  def apply[T](): ComposableSyncChannel[T] = new Impl[T]:
    val canRead = new ComposableReadSource
    val canSend = new ComposableSendSource

  abstract class Impl[T] extends SyncChannel.Impl[T], ComposableSyncChannel[T]:

    override protected def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      pending.iterator.find(op) match
        case Some(elem) => pending -= elem; true
        case None => false

    class ComposableReadSource extends ReadSource, Async.ComposableSource[T]
    class ComposableSendSource extends SendSource, Async.ComposableSource[Listener[T]]
  end Impl

end ComposableSyncChannel
