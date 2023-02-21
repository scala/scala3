package concurrent

import scala.collection.mutable, mutable.ListBuffer
import fiberRuntime.suspend
import fiberRuntime.boundary
import scala.compiletime.uninitialized
import scala.util.{Try, Success, Failure}
import scala.annotation.unchecked.uncheckedVariance
import java.util.concurrent.CancellationException

/** A cancellable future that can suspend waiting for other asynchronous sources
 */
trait Future[+T] extends Async.OriginalSource[Try[T]], Cancellable:

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def value(using async: Async): T

  /** Eventually stop computation of this future and fail with
   *  a `Cancellation` exception.
   */
  def cancel(): Unit

object Future:

  /**  A future that is completed explicitly by calling its
   *  `complete` method. There are two public implementations
   *
   *   - RunnableFuture: Completion is done by running a block of code
   *   - Promise.future: Completion is done by external request.
   */
  private class CoreFuture[+T] extends Future[T]:

    @volatile protected var hasCompleted: Boolean = false
    protected var cancelRequest = false
    private var result: Try[T] = uninitialized // guaranteed to be set if hasCompleted = true
    private val waiting: mutable.Set[Try[T] => Boolean] = mutable.Set()

    // Async.Source method implementations

    def poll(k: Async.Listener[Try[T]]): Boolean =
      hasCompleted && k(result)

    def addListener(k: Async.Listener[Try[T]]): Unit = synchronized:
      waiting += k

    def dropListener(k: Async.Listener[Try[T]]): Unit = synchronized:
      waiting -= k

    // Cancellable method implementations

    def cancel(): Unit =
      cancelRequest = true

    // Future method implementations

    def value(using async: Async): T =
      async.await(this).get

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
      val toNotify = synchronized:
        if hasCompleted then Nil
        else
          this.result = result
          hasCompleted = true
          val ws = waiting.toList
          waiting.clear()
          ws
      for listener <- toNotify do listener(result)

  end CoreFuture

  /** A future that is completed by evaluating `body` as a separate
   *  asynchronous operation in the given `scheduler`
   */
  private class RunnableFuture[+T](body: Async ?=> T)(using ac: Async.Config)
  extends CoreFuture[T]:

    /** a handler for Async */
    private def async(body: Async ?=> Unit): Unit =
      class FutureAsync(val scheduler: Scheduler, val group: Cancellable.Group) extends Async:

        def checkCancellation() =
          if cancelRequest then throw CancellationException()

        /** Await a source first by polling it, and, if that fails, by suspending
         *  in a onComplete call.
         */
        def await[T](src: Async.Source[T]): T =
          checkCancellation()
          src.poll().getOrElse:
            try
              var result: Option[T] = None // Not needed if we have full continuations
              suspend[T, Unit]: k =>
                src.onComplete: x =>
                  scheduler.schedule: () =>
                    result = Some(x)
                    k.resume()
                  true // signals to `src` that result `x` was consumed
              result.get
              /* With full continuations, the try block can be written more simply as follows:

                suspend[T, Unit]: k =>
                  src.onComplete: x =>
                    scheduler.schedule: () =>
                      k.resume(x)
                  true
              */
            finally checkCancellation()

        def withGroup(group: Cancellable.Group) = FutureAsync(scheduler, group)

      boundary [Unit]:
        body(using FutureAsync(ac.scheduler, ac.group))
    end async

    ac.scheduler.schedule: () =>
      async:
        link()
        Async.group:
          complete(Try(body))
        unlink()

  end RunnableFuture

  /** Create a future that asynchronously executes `body` that defines
   *  its result value in a Try or returns failure if an exception was thrown.
   *  If the future is created in an Async context, it is added to the
   *  children of that context's root.
   */
  def apply[T](body: Async ?=> T)(using ac: Async.Config): Future[T] =
    RunnableFuture(body)

  extension [T](f1: Future[T])

    /** Parallel composition of two futures.
     *  If both futures succeed, succeed with their values in a pair. Otherwise,
     *  fail with the failure that was returned first and cancel the other.
     */
    def zip[U](f2: Future[U])(using Async.Config): Future[(T, U)] = Future:
      Async.await(Async.either(f1, f2)) match
        case Left(Success(x1))  => (x1, f2.value)
        case Right(Success(x2)) => (f1.value, x2)
        case Left(Failure(ex))  => f2.cancel(); throw ex
        case Right(Failure(ex)) => f1.cancel(); throw ex

    /** Alternative parallel composition of this task with `other` task.
     *  If either task succeeds, succeed with the success that was returned first
     *  and cancel the other. Otherwise, fail with the failure that was returned last.
     */
    def alt(f2: Future[T], name: String = "alt")(using Async.Config): Future[T] = Future:
      boundary.setName(name)
      Async.await(Async.either(f1, f2)) match
        case Left(Success(x1))    => f2.cancel(); x1
        case Right(Success(x2))   => f1.cancel(); x2
        case Left(_: Failure[?])  => f2.value
        case Right(_: Failure[?]) => f1.value

  end extension

  // TODO: efficient n-ary versions of the last two operations

  /** A promise defines a future that is be completed via the
   *  promise's `complete` method.
   */
  class Promise[T]:
    private val myFuture = CoreFuture[T]()

    /** The future defined by this promise */
    val future: Future[T] = myFuture

    /** Define the result value of `future`. However, if `future` was
     *  cancelled in the meantime complete with a `CancellationException`
     *  failure instead.
     */
    def complete(result: Try[T]): Unit = myFuture.complete(result)

  end Promise
end Future

/** A task is a template that can be turned into a runnable future
 *  Composing tasks can be referentially transparent.
 */
class Task[+T](val body: Async ?=> T):

  /** Start a future computed from the `body` of this task */
  def run(using Async.Config) = Future(body)

end Task

def add(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Future[Int] =
  val b = x.zip:
    Future:
      xs.headOption.toString

  val _: Future[(Int, String)] = b

  val c = x.alt:
    Future:
      b.value._1
  val _: Future[Int] = c

  Future:
    val f1 = Future:
      x.value * 2
    x.value + xs.map(_.value).sum

end add

def Main(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Int =
  Async.blocking(add(x, xs).value)

