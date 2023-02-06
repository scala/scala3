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
  given fromAsync(using async: Async): Scheduler = async.client.scheduler
end Scheduler

trait Async:

  /** Wait for completion of future `f`. This means:
   *   - ensure that computing `f` has started
   *   - wait for the completion and return the completed Try
   */
  def await[T](f: Future[T]): Try[T]

  /** Wait for completion of the first of the futures `f1`, `f2`
   *  @return  `Left(r1)`  if `f1` completed first with `r1`
   *           `Right(r2)` if `f2` completed first with `r2`
  */
  def awaitEither[T1, T2](f1: Future[T1], f2: Future[T2]): Either[Try[T1], Try[T2]]

  /** The future computed by this async computation. */
  def client: Future[?]

object Async:
  inline def current(using async: Async): Async = async
end Async

class Future[+T](body: Async ?=> T)(using val scheduler: Scheduler):
  import Future.{Status, Cancellation}, Status.*

  @volatile private var status: Status = Started
  private var result: Try[T] = uninitialized
  private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()
  private var children: mutable.Set[Future[?]] = mutable.Set()

  private def addWaiting(k: Try[T] => Unit): Unit = synchronized:
    if status == Completed then k(result)
    else waiting += k

  private def currentWaiting(): List[Try[T] => Unit] = synchronized:
    val ws = waiting.toList
    waiting.clear()
    ws

  private def currentChildren(): List[Future[?]] = synchronized:
    val cs = children.toList
    children.clear()
    cs

  private def checkCancellation(): Unit =
    if status == Cancelled then throw Cancellation()

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def value(using async: Async): T = async.await(this).get

  // a handler for Async
  private def async(body: Async ?=> Unit): Unit =
    boundary [Unit]:
      given Async with

        private def resultOption[T](f: Future[T]): Option[Try[T]] = f.status match
          case Started =>
            None
          case Completed =>
            Some(f.result)
          case Cancelled =>
            Some(Failure(Cancellation()))

        private inline def cancelChecked[T](op: => T): T =
          checkCancellation()
          val res = op
          checkCancellation()
          res

        def await[T](f: Future[T]): Try[T] =
          cancelChecked:
            resultOption(f).getOrElse:
              suspend[Try[T], Unit]: s =>
                f.addWaiting: result =>
                  scheduler.schedule: () =>
                    s.resume(result)

        def awaitEither[T1, T2](f1: Future[T1], f2: Future[T2]): Either[Try[T1], Try[T2]] =
          cancelChecked:
            resultOption(f1).map(Left(_)).getOrElse:
              resultOption(f2).map(Right(_)).getOrElse:
                suspend[Either[Try[T1], Try[T2]], Unit]: s =>
                  var found = AtomicBoolean()
                  f1.addWaiting: result =>
                    if !found.getAndSet(true) then
                      scheduler.schedule: () =>
                        s.resume(Left(result))
                  f2.addWaiting: result =>
                    if !found.getAndSet(true) then
                      scheduler.schedule: () =>
                        s.resume(Right(result))

        def client = Future.this
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
    cancelChildren()
    notifyAll()

  /** Links the future as a child to the current async client.
   *  This means the future will be cancelled when the async client
   *  completes.
   */
  def linked(using async: Async): this.type = synchronized:
    if status != Completed then
      async.client.children += this
    this

  private def cancelChildren(): Unit =
    for f <- currentChildren() do f.cancel()

  /** Eventually stop computation of this future and fail with
   *  a `Cancellation` exception. Also cancel all linked children.
   */
  def cancel(): Unit = synchronized:
    if status != Completed && status != Cancelled then
      status = Cancelled
      cancelChildren()

  /** Block thread until future is completed and return result
   *  N.B. This should be parameterized with a timeout.
   */
  def force(): T =
    while status != Completed do wait()
    result.get

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

def Test(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Future[Int] =
  Future:
    x.value + xs.map(_.value).sum

def Main(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Int =
  Test(x, xs).force()

