package concurrent

import scala.collection.mutable, mutable.ListBuffer
import scala.util.boundary
import scala.compiletime.uninitialized
import scala.util.{Try, Success, Failure}
import scala.annotation.unchecked.uncheckedVariance
import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.AtomicBoolean
import runtime.suspend

trait Future[+T] extends Async.Source[Try[T]], Cancellable:

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def value(using async: Async): T

  /** Block thread until future is completed and return result
   *  N.B. This should be parameterized with a timeout.
   */
  def force(): T

  /** Links the future as a child to the current async runner.
   *  This means the future will be cancelled when the async runner
   *  completes.
   */
  def linked(using async: Async): this.type

  /** Eventually stop computation of this future and fail with
   *  a `Cancellation` exception. Also cancel all linked children.
   */
  def cancel(): Unit

object Future:

  private enum Status:
    // Transitions always go left to right.
    // Cancelled --> Completed with Failure(CancellationException()) result
    case Started, Cancelled, Completed
  import Status.*

  private class CoreFuture[+T] extends Future[T]:
    @volatile protected var status: Status = Started
    private var result: Try[T] = uninitialized
    private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()
    private var children: mutable.Set[Cancellable] = mutable.Set()

    private def currentWaiting(): List[Try[T] => Unit] = synchronized:
      val ws = waiting.toList
      waiting.clear()
      ws

    private def currentChildren(): List[Cancellable] = synchronized:
      val cs = children.toList
      children.clear()
      cs

    def poll: Option[Try[T]] =
      if status == Started then None else Some(result)

    def handleWith(k: Try[T] => Unit): Unit = synchronized:
      if status == Started then waiting += k else k(result)

    def cancel(): Unit =
      val toCancel = synchronized:
        if status != Completed && status != Cancelled then
          result = Failure(new CancellationException())
          status = Cancelled
          currentChildren()
        else
          Nil
      toCancel.foreach(_.cancel())

    def addChild(child: Cancellable): Unit = synchronized:
      if status == Completed then child.cancel()
      else children += this

    def isCancelled = status == Cancelled

    def linked(using async: Async): this.type =
      if status != Completed then async.runner.addChild(this)
      this

    def value(using async: Async): T = async.await(this).get

    def force(): T =
      while status != Completed do wait()
      result.get

    /** Complete future with result. If future was cancelled in the meantime,
     *  return a CancellationException failure instead.
     *  Note: @uncheckedVariance is safe here since `complete` is called from
     *  only two places:
     *   - from the initializer of RunnableFuture, where we are sure that `T`
     *     is exactly the type with which the future was created, and
     *   - from Promise.complete, where we are sure the type `T` is exactly
     *     the type with which the future was created since `Promise` is invariant.
     */
    private[Future] def complete(result: Try[T] @uncheckedVariance): Unit =
      if status == Started then this.result = result
      status = Completed
      for task <- currentWaiting() do task(result)
      notifyAll()

  end CoreFuture

  private class RunnableFuture[+T](body: Async ?=> T)(using scheduler: Scheduler)
  extends CoreFuture[T]:

    // a handler for Async
    private def async(body: Async ?=> Unit): Unit =
      boundary [Unit]:
        given Async with
          private def checkCancellation(): Unit =
            if status == Cancelled then throw new CancellationException()

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

          def runner: Cancellable = RunnableFuture.this
          def scheduler = RunnableFuture.this.scheduler
        end given

        body
    end async

    scheduler.schedule: () =>
      async:
        complete(
          try Success(body)
          catch case ex: Exception => Failure(ex))
  end RunnableFuture

  /** Create a future that asynchronously executes `body` to define
   *  its result value in a Try or returns failure if an exception was thrown.
   */
  def apply[T](body: Async ?=> T)(using Scheduler): Future[T] = RunnableFuture(body)

  /** A promise defines a future that is be completed via the
   *  promise's `complete` method.
   */
  class Promise[T]:
    private val myFuture = CoreFuture[T]()

    /** The future defined by this promise */
    def future: Future[T] = myFuture

    /** Define the result value of `future`. However, if `future` was
     *  cancelled in the meantime complete with a `CancellationException`
     *  failure instead.
     */
    def complete(result: Try[T]): Unit = myFuture.complete(result)

  end Promise
end Future

/** A task is a template that can be turned into a runnable future */
class Task[+T](val body: Async ?=> T):

  /** Start a future computed from the `body` of this task */
  def run(using Scheduler): Future[T] = Future(body)

  /** Parallel composition of this task with `other` task.
   *  If both tasks succeed, succeed with their values in a pair. Otherwise,
   *  fail with the failure that was returned first.
   */
  def par[U](other: Task[U]): Task[(T, U)] =
    Task: async ?=>
      val f1 = Future(this.body).linked
      val f2 = Future(other.body).linked
      async.awaitEither(f1, f2) match
        case Left(Success(x1))  => (x1, f2.value)
        case Right(Success(x2)) => (f1.value, x2)
        case Left(Failure(ex))  => throw ex
        case Right(Failure(ex)) => throw ex

  /** Alternative parallel composition of this task with `other` task.
   *  If either task succeeds, succeed with the success that was returned first.
   *  Otherwise, fail with the failure that was returned last.
   */
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

