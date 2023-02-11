package concurrent
import scala.collection.mutable, mutable.ListBuffer
import scala.util.boundary, boundary.Label
import runtime.suspend
import java.util.concurrent.CancellationException
import Async.{Listener, await}

/** An unbounded asynchronous channel. Senders do not wait for matching
 *  readers.
 */
class UnboundedChannel[T] extends Async.Source[T]:

  private val pending = ListBuffer[T]()
  private val waiting = mutable.Set[Listener[T]]()

  private def drainWaiting(x: T): Boolean =
    waiting.iterator.find(_(x)) match
      case Some(k) => waiting -= k; true
      case None => false

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
 *  until a communication between them happens. The channel provides two
 *  async sources, one for reading and one for sending. If a send operation
 *  encounters some waiting readers, or a read operation encounters some
 *  waiting sender the data is transmitted directly. Otherwise we add
 *  the operation to the corresponding pending set.
 */
trait SyncChannel[T]:

  val canRead: Async.Source[T]
  val canSend: Async.Source[Listener[T]]

  def send(x: T)(using Async): Unit = await(canSend)(x)

  def read()(using Async): T = await(canRead)

object SyncChannel:

  def apply[T](): SyncChannel[T] = new SyncChannel[T]:

    private val pendingReads = mutable.Set[Listener[T]]()
    private val pendingSends = mutable.Set[Listener[Listener[T]]]()

    private def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      // Since sources are filterable, we have to match all pending readers or writers
      // against the incoming request
      pending.iterator.find(op) match
        case Some(elem) => pending -= elem; true
        case None => false

    private def collapse[T](k2: Listener[Listener[T]]): Option[T] =
      var r: Option[T] = None
      if k2 { x => r = Some(x); true } then r else None

    val canRead = new Async.Source[T]:
      def poll(k: Listener[T]): Boolean =
        link(pendingSends, sender => collapse(sender).map(k) == Some(true))
      def onComplete(k: Listener[T]): Unit =
        if !poll(k) then pendingReads += k
      def dropListener(k: Listener[T]): Unit =
        pendingReads -= k

    val canSend = new Async.Source[Listener[T]]:
      def poll(k: Listener[Listener[T]]): Boolean =
        link(pendingReads, k(_))
      def onComplete(k: Listener[Listener[T]]): Unit =
        if !poll(k) then pendingSends += k
      def dropListener(k: Listener[Listener[T]]): Unit =
        pendingSends -= k

end SyncChannel

/** A simplistic coroutine. Error handling is still missing,  */
class Coroutine(body: Async ?=> Unit)(using scheduler: Scheduler) extends Cancellable:
  private var children: mutable.ListBuffer[Cancellable] = mutable.ListBuffer()
  @volatile var cancelled = false

  def cancel() =
    cancelled = true
    synchronized(children).foreach(_.cancel())

  def addChild(child: Cancellable) = synchronized:
    children += child

  boundary [Unit]:
    given Async = new Async.Impl(this, scheduler):
      def checkCancellation() =
        if cancelled then throw new CancellationException()
    try body
    catch case ex: CancellationException => ()
end Coroutine

def TestChannel(using Scheduler) =
  val c = SyncChannel[Option[Int]]()
  Coroutine:
    for i <- 0 to 100 do
      c.send(Some(i))
      c.send(None)
  Coroutine:
    var sum = 0
    def loop(): Unit =
      c.read() match
        case Some(x) => sum += x; loop()
        case None => println(sum)
    loop()

def TestRace =
  val c1, c2 = SyncChannel[Int]()
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

    val d5 = Async.either(c1.canRead, d2)
    .map:
      case Left(x) => -x
      case Right(x) => x

