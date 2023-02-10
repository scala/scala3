package concurrent
import scala.collection.mutable, mutable.ListBuffer
import scala.util.boundary.Label
import runtime.suspend
import Async.{Listener, await, Yes}

/** An unbounded channel
 *  Unbounded channels are composable async sources.
 */
class UnboundedChannel[T] extends Async.Source[T]:
  type CanFilter = Yes

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

/** An unbuffered, synchronous channel. Senders and readers both block
 *  until a communication between them happens.
 *  The channel provides two async sources, one for reading and one for
 *  sending. The two sources are not composable. This allows a simple
 *  implementation strategy where at each point either some senders
 *  are waiting for matching readers, or some readers are waiting for matching
 *  senders, or the channel is idle, i.e. there are no waiting readers or senders.
 *  If a send operation encounters some waiting readers, or a read operation
 *  encounters some waiting sender the data is transmitted directly. Otherwise
 *  we add the operation to the corresponding waiting pending set.
 */
trait SyncChannel[T]:
  thisCannel =>

  type CanFilter

  val canRead: Async.Source[T]           { type CanFilter = thisCannel.CanFilter }
  val canSend: Async.Source[Listener[T]] { type CanFilter = thisCannel.CanFilter }

  def send(x: T)(using Async): Unit = await(canSend)(x)

  def read()(using Async): T = await(canRead)

object SyncChannel:
  def apply[T](): SyncChannel[T] = Impl[T]()

  class Impl[T] extends SyncChannel[T]:

    private val pendingReads = mutable.Set[Listener[T]]()
    private val pendingSends = mutable.Set[Listener[Listener[T]]]()

    protected def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      pending.headOption match
        case Some(elem) =>
          val ok = op(elem)
          if !ok then
            // Since sources are not filterable, we can be here only if a race
            // was lost and the entry was not yet removed. In that case, remove
            // it here.
            pending -= pending.head
            link(pending, op)
          ok
        case None => false

    private def collapse[T](k2: Listener[Listener[T]]): Option[T] =
      var r: Option[T] = None
      if k2 { x => r = Some(x); true } then r else None

    private class ReadSource extends Async.Source[T]:
      type CanFilter = Impl.this.CanFilter
      def poll(k: Listener[T]): Boolean =
        link(pendingSends, sender => collapse(sender).map(k) == Some(true))
      def onComplete(k: Listener[T]): Unit =
        if !poll(k) then pendingReads += k
      def dropListener(k: Listener[T]): Unit =
        pendingReads -= k

    private class SendSource extends Async.Source[Listener[T]]:
      type CanFilter = Impl.this.CanFilter
      def poll(k: Listener[Listener[T]]): Boolean =
        link(pendingReads, k(_))
      def onComplete(k: Listener[Listener[T]]): Unit =
        if !poll(k) then pendingSends += k
      def dropListener(k: Listener[Listener[T]]): Unit =
        pendingSends -= k

    val canRead = new ReadSource
    val canSend = new SendSource
  end Impl
end SyncChannel

object FilterableSyncChannel:
  def apply[T](): SyncChannel[T] { type CanFilter = Yes } = Impl[T]()

  class Impl[T] extends SyncChannel.Impl[T]:
    type CanFilter = Yes
    override protected def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      // Since sources are filterable, we have to match all pending readers or writers
      // against the incoming request
      pending.iterator.find(op) match
        case Some(elem) => pending -= elem; true
        case None => false

end FilterableSyncChannel

def TestRace =
  val c1, c2 = FilterableSyncChannel[Int]()
  val s = c1.canSend
  val c3 = Async.race(c1.canRead, c2.canRead)
  val c4 = c3.filter(_ >= 0)
  val d0 = SyncChannel[Int]()
  val d1 = Async.race(c1.canRead, c2.canRead, d0.canRead)
  val d2 = d1.map(_ + 1)
  val c5 = Async.either(c1.canRead, c2.canRead)
    .map:
      case Left(x) => -x
      case Right(x) => x
    .filter(_ >= 0)

  //val d3bad = d1.filter(_ >= 0)
  val d5 = Async.either(c1.canRead, d2)
    .map:
      case Left(x) => -x
      case Right(x) => x
  //val d6bad = d5.filter(_ >= 0)
