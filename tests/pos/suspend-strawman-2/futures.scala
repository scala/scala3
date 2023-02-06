package futures

import scala.collection.mutable, mutable.ListBuffer
import scala.util.boundary, boundary.Label
import scala.compiletime.uninitialized
import scala.util.{Try, Success, Failure}
import java.util.concurrent.atomic.AtomicBoolean
import runtime.suspend

/** A hypothetical task scheduler trait */
trait Scheduler:
  def schedule(task: Runnable): Unit = ???

object Scheduler extends Scheduler:
  given fromAsync(using async: Async): Scheduler = async.runner.scheduler
end Scheduler

/** A context that allows one to suspend waiting for asynchronous data sources */
trait Async:

  /** Wait for completion of async source `src` and return the result */
  def await[T](src: Async.Source[T]): T

  /** Wait for completion of the first of the sources `src1`, `src2`
   *  @return  `Left(r1)`  if `src1` completed first with `r1`
   *           `Right(r2)` if `src2` completed first with `r2`
  */
  def awaitEither[T1, T2](src1: Async.Source[T1], src2: Async.Source[T2]): Either[T1, T2]

  /** The runner underlying this async computation. */
  def runner: Async.Runner

object Async:

  /** The currently executing Async context */
  inline def current(using async: Async): Async = async

  /** An asynchronous data source. Sources can be persistent or ephemeral.
   *  A persistent source will always return the same data to calls to `poll`
   *  and pass the same data to calls of `handle`. An ephemeral source might pass new
   *  data in every call. An example of a persistent source is  `Future`. An
   *  example of an ephemeral source is `Channel`.
   */
  trait Source[+T]:

    /** Poll whether data is available
     *  @return  The data or None in an option. Depending on the nature of the
     *           source, data might be returned only once in a poll. E.g. if
     *           the source is a channel, a Some result might skip to the next
     *           entry.
     */
    def poll: Option[T]

    /** When data is available, pass it to function `k`.
     */
    def handleWith(k: T => Unit): Unit

  end Source

  /** A thread-like entity that can be cancelled */
  trait Runner:

    /** The scheduler on which this computation is running */
    def scheduler: Scheduler

    /** Cancel computation for this runner and all its children */
    def cancel(): Unit

    /** Add a given child to this runner */
    def addChild(child: Runner): Unit
  end Runner

end Async


class Future[+T](body: Async ?=> T)(using val scheduler: Scheduler)
extends Async.Source[Try[T]], Async.Runner:
  import Future.{Status, Cancellation}, Status.*

  @volatile private var status: Status = Started
  private var result: Try[T] = uninitialized
  private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()
  private var children: mutable.Set[Async.Runner] = mutable.Set()

  private def currentWaiting(): List[Try[T] => Unit] = synchronized:
    val ws = waiting.toList
    waiting.clear()
    ws

  private def currentChildren(): List[Async.Runner] = synchronized:
    val cs = children.toList
    children.clear()
    cs

  def poll: Option[Try[T]] = status match
    case Started   =>  None
    case Completed => Some(result)
    case Cancelled => Some(Failure(Cancellation()))

  def handleWith(k: Try[T] => Unit): Unit = synchronized:
    if status == Completed then k(result)
    else waiting += k

  /** Eventually stop computation of this future and fail with
   *  a `Cancellation` exception. Also cancel all linked children.
   */
  def cancel(): Unit = synchronized:
    if status != Completed && status != Cancelled then
      status = Cancelled
      for f <- currentChildren() do f.cancel()

  def addChild(child: Async.Runner): Unit = synchronized:
    if status == Completed then child.cancel()
    else children += this

  /** Links the future as a child to the current async client.
   *  This means the future will be cancelled when the async client
   *  completes.
   */
  def linked(using async: Async): this.type = synchronized:
    if status != Completed then async.runner.addChild(this)
    this

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def value(using async: Async): T = async.await(this).get

  /** Block thread until future is completed and return result
   *  N.B. This should be parameterized with a timeout.
   */
  def force(): T =
    while status != Completed do wait()
    result.get

  // a handler for Async
  private def async(body: Async ?=> Unit): Unit =
    boundary [Unit]:
      given Async with
        private def checkCancellation(): Unit =
          if status == Cancelled then throw Cancellation()

        private inline def cancelChecked[T](op: => T): T =
          checkCancellation()
          val res = op
          checkCancellation()
          res

        def await[T](src: Async.Source[T]): T =
          cancelChecked:
            src.poll.getOrElse:
              suspend[T, Unit]: k =>
                src.handleWith: result =>
                  scheduler.schedule: () =>
                    k.resume(result)

        def awaitEither[T1, T2](src1: Async.Source[T1], src2: Async.Source[T2]): Either[T1, T2] =
          cancelChecked:
            src1.poll.map(Left(_)).getOrElse:
              src2.poll.map(Right(_)).getOrElse:
                suspend[Either[T1, T2], Unit]: k =>
                  var found = AtomicBoolean()
                  src1.handleWith: result =>
                    if !found.getAndSet(true) then
                      scheduler.schedule: () =>
                        k.resume(Left(result))
                  src2.handleWith: result =>
                    if !found.getAndSet(true) then
                      scheduler.schedule: () =>
                        k.resume(Right(result))

        def runner = Future.this
      end given

      body
  end async

  private def complete(): Unit =
    async:
      result =
        try Success(body)
        catch case ex: Exception => Failure(ex)
      status = Completed
    for task <- currentWaiting() do task(result)
    cancel()
    notifyAll()

  scheduler.schedule(() => complete())
end Future

object Future:
  enum Status:
    // Transitions always go left to right.
    // Cancelled --> Completed with Failure(Cancellation()) result
    case Started, Cancelled, Completed

  class Cancellation extends Exception
end Future

class Task[+T](val body: Async ?=> T):
  def run(using Scheduler): Future[T] = Future(body)

  def par[U](other: Task[U]): Task[(T, U)] =
    Task: async ?=>
      val f1 = Future(this.body).linked
      val f2 = Future(other.body).linked
      async.awaitEither(f1, f2) match
        case Left(Success(x1))  => (x1, f2.value)
        case Right(Success(x2)) => (f1.value, x2)
        case Left(Failure(ex))  => throw ex
        case Right(Failure(ex)) => throw ex

  def alt[U >: T](other: Task[U]): Task[U] =
    Task: async ?=>
      val f1 = Future(this.body).linked
      val f2 = Future(other.body).linked
      async.awaitEither(f1, f2) match
        case Left(Success(x1))    => x1
        case Right(Success(x2))   => x2
        case Left(_: Failure[?])  => f2.value
        case Right(_: Failure[?]) => f1.value
end Task

/** An unbounded channel */
class Channel[T] extends Async.Source[T]:
  private val pending = ListBuffer[T]()
  private val waiting = ListBuffer[T => Unit]()
  def send(x: T): Unit = synchronized:
    if waiting.isEmpty then pending += x
    else
      val k = waiting.head
      waiting.dropInPlace(1)
      k(x)
  def poll: Option[T] = synchronized:
    if pending.isEmpty then None
    else
      val x = pending.head
      pending.dropInPlace(1)
      Some(x)
  def handleWith(k: T => Unit): Unit = synchronized:
    if pending.isEmpty then waiting += k
    else
      val x = pending.head
      pending.dropInPlace(1)
      k(x)
end Channel

def Test(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Future[Int] =
  Future:
    x.value + xs.map(_.value).sum

def Main(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Int =
  Test(x, xs).force()

