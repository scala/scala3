package concurrent
import scala.collection.mutable, mutable.ListBuffer
import fiberRuntime.boundary, boundary.Label
import fiberRuntime.suspend
import scala.concurrent.ExecutionContext
import scala.util.{Try, Failure}
import Async.{Listener, await}

/** A common interface for channels */
trait Channel[T]:
  def read()(using Async): T
  def send(x: T)(using Async): Unit
  protected def shutDown(finalValue: T): Unit

object Channel:

  extension [T](c: Channel[Try[T]])
    def close(): Unit =
      c.shutDown(Failure(ChannelClosedException()))

class ChannelClosedException extends Exception

/** An unbounded asynchronous channel. Senders do not wait for matching
 *  readers.
 */
class AsyncChannel[T] extends Async.OriginalSource[T], Channel[T]:

  private val pending = ListBuffer[T]()
  private val waiting = mutable.Set[Listener[T]]()
  private var isClosed = false

  private def ensureOpen() =
    if isClosed then throw ChannelClosedException()

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

  def send(x: T)(using Async): Unit = synchronized:
    ensureOpen()
    val sent = pending.isEmpty && drainWaiting(x)
    if !sent then pending += x

  def poll(k: Listener[T]): Boolean = synchronized:
    ensureOpen()
    drainPending(k)

  def addListener(k: Listener[T]): Unit = synchronized:
    waiting += k

  def dropListener(k: Listener[T]): Unit = synchronized:
    waiting -= k

  protected def shutDown(finalValue: T) =
    isClosed = true
    waiting.foreach(_(finalValue))

end AsyncChannel

/** An unbuffered, synchronous channel. Senders and readers both block
 *  until a communication between them happens. The channel provides two
 *  async sources, one for reading and one for sending. If a send operation
 *  encounters some waiting readers, or a read operation encounters some
 *  waiting sender the data is transmitted directly. Otherwise we add
 *  the operation to the corresponding pending set.
 */
trait SyncChannel[T] extends Channel[T]:

  val canRead: Async.Source[T]
  val canSend: Async.Source[Listener[T]]

  def send(x: T)(using Async): Unit = await(canSend)(x)

  def read()(using Async): T = await(canRead)

object SyncChannel:

  def apply[T](): SyncChannel[T] = new SyncChannel[T]:

    private val pendingReads = mutable.Set[Listener[T]]()
    private val pendingSends = mutable.Set[Listener[Listener[T]]]()
    private var isClosed = false

    private def ensureOpen() =
      if isClosed then throw ChannelClosedException()

    private def link[T](pending: mutable.Set[T], op: T => Boolean): Boolean =
      ensureOpen()
      // Since sources are filterable, we have to match all pending readers or writers
      // against the incoming request
      pending.iterator.find(op) match
        case Some(elem) => pending -= elem; true
        case None => false

    private def collapse[T](k2: Listener[Listener[T]]): Option[T] =
      var r: Option[T] = None
      if k2 { x => r = Some(x); true } then r else None

    val canRead = new Async.OriginalSource[T]:
      def poll(k: Listener[T]): Boolean =
        link(pendingSends, sender => collapse(sender).map(k) == Some(true))
      def addListener(k: Listener[T]) = synchronized:
        pendingReads += k
      def dropListener(k: Listener[T]): Unit = synchronized:
        pendingReads -= k

    val canSend = new Async.OriginalSource[Listener[T]]:
      def poll(k: Listener[Listener[T]]): Boolean =
        link(pendingReads, k(_))
      def addListener(k: Listener[Listener[T]]) = synchronized:
        pendingSends += k
      def dropListener(k: Listener[Listener[T]]): Unit = synchronized:
        pendingSends -= k

    protected def shutDown(finalValue: T) =
      isClosed = true
      pendingReads.foreach(_(finalValue))

end SyncChannel

def TestChannel(using ExecutionContext) =
  val c = SyncChannel[Option[Int]]()
  Future:
    for i <- 0 to 100 do
      c.send(Some(i))
      c.send(None)
  Future:
    var sum = 0
    def loop(): Unit =
      c.read() match
        case Some(x) => sum += x; loop()
        case None => println(sum)
    loop()
  val chan = SyncChannel[Int]()
  val allTasks = List(
      Task:
        println("task1")
        chan.read(),
      Task:
        println("task2")
        chan.read()
    )

  def start() = Future:
    allTasks.map(_.run.value).sum

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

