package gears.async

import language.experimental.captureChecking

import java.util.concurrent.CancellationException
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import gears.async.Async.SourceSymbol

/** Futures are [[Async.Source Source]]s that has the following properties:
  *   - They represent a single value: Once resolved, [[Async.await await]]-ing on a [[Future]] should always return the
  *     same value.
  *   - They can potentially be cancelled, via [[Cancellable.cancel the cancel method]].
  *
  * There are two kinds of futures, active and passive.
  *   - '''Active''' futures are ones that are spawned with [[Future.apply]] and [[Task.start]]. They require the
  *     [[Async.Spawn]] context, and run on their own (as long as the [[Async.Spawn]] scope has not ended). Active
  *     futures represent concurrent computations within Gear's structured concurrency tree. Idiomatic Gears code should
  *     ''never'' return active futures. Should a function be async (i.e. takes an [[Async]] context parameter), they
  *     should return values or throw exceptions directly.
  *   - '''Passive''' futures are ones that are created by [[Future.Promise]] (through
  *     [[Future.Promise.asFuture asFuture]]) and [[Future.withResolver]]. They represent yet-arrived values coming from
  *     ''outside'' of Gear's structured concurrency tree (for example, from network or the file system, or even from
  *     another concurrency system like [[scala.concurrent.Future Scala standard library futures]]). Idiomatic Gears
  *     libraries should return this kind of [[Future]] if deemed neccessary, but functions returning passive futures
  *     should ''not'' take an [[Async]] context.
  *
  * @see
  *   [[Future.apply]] and [[Task.start]] for creating active futures.
  * @see
  *   [[Future.Promise]] and [[Future.withResolver]] for creating passive futures.
  * @see
  *   [[Future.awaitAll]], [[Future.awaitFirst]] and [[Future.Collector]] for tools to work with multiple futures.
  * @see
  *   [[ScalaConverters.asGears]] and [[ScalaConverters.asScala]] for converting between Scala futures and Gears
  *   futures.
  */
trait Future[+T] extends Async.OriginalSource[Try[T]], Cancellable

object Future:
  /** A future that is completed explicitly by calling its `complete` method. There are three public implementations
    *
    *   - RunnableFuture: Completion is done by running a block of code
    *   - Promise.apply: Completion is done by external request.
    *   - withResolver: Completion is done by external request set up from a block of code.
    */
  private class CoreFuture[+T] extends Future[T]:

    @volatile protected var hasCompleted: Boolean = false
    protected var cancelRequest = AtomicBoolean(false)
    private var result: Try[T] = uninitialized // guaranteed to be set if hasCompleted = true
    private val waiting: mutable.Set[Listener[Try[T]]] = mutable.Set()

    // Async.Source method implementations

    import caps.unsafe.unsafeAssumePure

    def poll(k: Listener[Try[T]]^): Boolean =
      if hasCompleted then
        k.completeNow(result, this)
        true
      else false

    def addListener(k: Listener[Try[T]]^): Unit = synchronized:
      waiting += k.unsafeAssumePure

    def dropListener(k: Listener[Try[T]]^): Unit = synchronized:
      waiting -= k.unsafeAssumePure

    // Cancellable method implementations

    def cancel(): Unit =
      setCancelled()

    override def link(group: CompletionGroup): this.type =
      // though hasCompleted is accessible without "synchronized",
      // we want it not to be run while the future was trying to complete.
      synchronized:
        if !hasCompleted || group == CompletionGroup.Unlinked then super.link(group)
        else this

    /** Sets the cancellation state and returns `true` if the future has not been completed and cancelled before. */
    protected final def setCancelled(): Boolean =
      !hasCompleted && cancelRequest.compareAndSet(false, true)

    /** Complete future with result. If future was cancelled in the meantime, return a CancellationException failure
      * instead. Note: @uncheckedVariance is safe here since `complete` is called from only two places:
      *   - from the initializer of RunnableFuture, where we are sure that `T` is exactly the type with which the future
      *     was created, and
      *   - from Promise.complete, where we are sure the type `T` is exactly the type with which the future was created
      *     since `Promise` is invariant.
      */
    private[Future] def complete(result: Try[T] @uncheckedVariance): Unit =
      val toNotify = synchronized:
        if hasCompleted then Nil
        else
          this.result = result
          hasCompleted = true
          val ws = waiting.toList
          waiting.clear()
          unlink()
          ws
      for listener <- toNotify do listener.completeNow(result, this)

  end CoreFuture

  /** A future that is completed by evaluating `body` as a separate asynchronous operation in the given `scheduler`
    */
  private class RunnableFuture[+T](body: Async.Spawn ?-> T)(using ac: Async) extends CoreFuture[T]:
    private given acSupport: ac.support.type = ac.support
    private given acScheduler: ac.support.Scheduler = ac.scheduler
    /** RunnableFuture maintains its own inner [[CompletionGroup]], that is separated from the provided Async
      * instance's. When the future is cancelled, we only cancel this CompletionGroup. This effectively means any
      * `.await` operations within the future is cancelled *only if they link into this group*. The future body run with
      * this inner group by default, but it can always opt-out (e.g. with [[uninterruptible]]).
      */
    private var innerGroup: CompletionGroup = CompletionGroup()

    private def checkCancellation(): Unit =
      if cancelRequest.get() then throw new CancellationException()

    private class FutureAsync[Cap^](val group: CompletionGroup)(using label: acSupport.Label[Unit, Cap])
        extends Async(using acSupport, acScheduler):
      /** Await a source first by polling it, and, if that fails, by suspending in a onComplete call.
        */
      override def await[U](src: Async.Source[U]^): U =
        class CancelSuspension extends Cancellable:
          var suspension: acSupport.Suspension[Try[U], Unit]^{Cap^} = uninitialized
          var listener: Listener[U]^{this, Cap^} = uninitialized
          var completed = false

          def complete() = synchronized:
            val completedBefore = completed
            completed = true
            completedBefore

          override def cancel() =
            val completedBefore = complete()
            if !completedBefore then
              src.dropListener(listener)
              // SAFETY: we always await for this suspension to end
              val pureSusp = caps.unsafe.unsafeAssumePure(suspension)
              acSupport.resumeAsync(pureSusp)(Failure(new CancellationException()))

        if group.isCancelled then throw new CancellationException()

        src
          .poll()
          .getOrElse:
            val cancellable = CancelSuspension()
            val res = acSupport.suspend[Try[U], Unit, Cap](k =>
              val listener = Listener.acceptingListener[U]: (x, _) =>
                val completedBefore = cancellable.complete()
                // SAFETY: Future should already capture Cap^
                val purek = caps.unsafe.unsafeAssumePure(k)
                if !completedBefore then acSupport.resumeAsync(purek)(Success(x))
              cancellable.suspension = k
              cancellable.listener = listener
              cancellable.link(group) // may resume + remove listener immediately
              src.onComplete(listener)
            )
            cancellable.unlink()
            res.get

      override def withGroup(group: CompletionGroup): Async = FutureAsync[Cap](group)

    override def cancel(): Unit = if setCancelled() then this.innerGroup.cancel()

    link()
    ac.support.scheduleBoundary:
      val result = Async.withNewCompletionGroup(innerGroup)(Try({
        val r = body
        checkCancellation()
        r
      }).recoverWith { case _: InterruptedException | _: CancellationException =>
        Failure(new CancellationException())
      })(using FutureAsync(CompletionGroup.Unlinked))
      complete(result)

  end RunnableFuture


  /** Create a future that asynchronously executes `body` that wraps its execution in a [[scala.util.Try]]. The returned
    * future is linked to the given [[Async.Spawn]] scope by default, i.e. it is cancelled when this scope ends.
    */
  def apply[T](body: Async.Spawn ?=> T)(using async: Async, spawnable: Async.Spawn)(
    using async.type =:= spawnable.type
  ): Future[T]^{body, spawnable} =
    val f = (async: Async.Spawn) => body(using async)
    val puref = caps.unsafe.unsafeAssumePure(f)
    // SAFETY: body is recorded in the capture set of Future, which should be cancelled when gone out of scope.
    RunnableFuture(async ?=> puref(async))(using spawnable)

  /** A future that is immediately completed with the given result. */
  def now[T](result: Try[T]): Future[T] =
    val f = CoreFuture[T]()
    f.complete(result)
    f

  /** An alias to [[now]]. */
  inline def completed[T](result: Try[T]) = now(result)

  /** A future that immediately resolves with the given result. Similar to `Future.now(Success(result))`. */
  inline def resolved[T](result: T): Future[T] = now(Success(result))

  /** A future that immediately rejects with the given exception. Similar to `Future.now(Failure(exception))`. */
  inline def rejected(exception: Throwable): Future[Nothing] = now(Failure(exception))

  extension [T](f1: Future[T]^)
    /** Parallel composition of two futures. If both futures succeed, succeed with their values in a pair. Otherwise,
      * fail with the failure that was returned first.
      */
    def zip[U](f2: Future[U]^): Future[(T, U)]^{f1, f2} =
      Future.withResolver[(T, U), caps.CapSet^{f1, f2}]: r =>
        Async
          .either(f1, f2)
          .onComplete(Listener { (v, _) =>
            v match
              case Left(Success(x1)) =>
                f2.onComplete(Listener { (x2, _) => r.complete(x2.map((x1, _))) })
              case Right(Success(x2)) =>
                f1.onComplete(Listener { (x1, _) => r.complete(x1.map((_, x2))) })
              case Left(Failure(ex))  => r.reject(ex)
              case Right(Failure(ex)) => r.reject(ex)
          })

    // /** Parallel composition of tuples of futures. Disabled since scaladoc is crashing with it. (https://github.com/scala/scala3/issues/19925) */
    // def *:[U <: Tuple](f2: Future[U]): Future[T *: U] = Future.withResolver: r =>
    //   Async
    //     .either(f1, f2)
    //     .onComplete(Listener { (v, _) =>
    //       v match
    //         case Left(Success(x1)) =>
    //           f2.onComplete(Listener { (x2, _) => r.complete(x2.map(x1 *: _)) })
    //         case Right(Success(x2)) =>
    //           f1.onComplete(Listener { (x1, _) => r.complete(x1.map(_ *: x2)) })
    //         case Left(Failure(ex))  => r.reject(ex)
    //         case Right(Failure(ex)) => r.reject(ex)
    //     })

    /** Alternative parallel composition of this task with `other` task. If either task succeeds, succeed with the
      * success that was returned first. Otherwise, fail with the failure that was returned last.
      * @see
      *   [[orWithCancel]] for an alternative version where the slower future is cancelled.
      */
    def or(f2: Future[T]^): Future[T]^{f1, f2} = orImpl(false)(f2)

    /** Like `or` but the slower future is cancelled. If either task succeeds, succeed with the success that was
      * returned first and the other is cancelled. Otherwise, fail with the failure that was returned last.
      */
    def orWithCancel(f2: Future[T]^): Future[T]^{f1, f2} = orImpl(true)(f2)

    inline def orImpl(inline withCancel: Boolean)(f2: Future[T]^): Future[T]^{f1, f2} = Future.withResolver[T, caps.CapSet^{f1, f2}]: r =>
      Async
        .raceWithOrigin(f1, f2)
        .onComplete(Listener { case ((v, which), _) =>
          v match
            case Success(value) =>
              inline if withCancel then (if which == f1 then f2 else f1).cancel()
              r.resolve(value)
            case Failure(_) =>
              (if which == f1.symbol then f2 else f1).onComplete(Listener((v, _) => r.complete(v)))
        })

  end extension

  /** A promise is a [[Future]] that is be completed manually via the `complete` method.
    * @see
    *   [[Promise$.apply]] to create a new, empty promise.
    * @see
    *   [[Future.withResolver]] to create a passive [[Future]] from callback-style asynchronous calls.
    */
  trait Promise[T] extends Future[T]:
    inline def asFuture: Future[T] = this

    /** Define the result value of `future`. */
    def complete(result: Try[T]): Unit

  object Promise:
    /** Create a new, unresolved [[Promise]]. */
    def apply[T](): Promise[T] =
      new CoreFuture[T] with Promise[T]:
        override def cancel(): Unit =
          if setCancelled() then complete(Failure(new CancellationException()))

        /** Define the result value of `future`. However, if `future` was cancelled in the meantime complete with a
          * `CancellationException` failure instead.
          */
        override def complete(result: Try[T]): Unit = super[CoreFuture].complete(result)
  end Promise

  /** The group of handlers to be used in [[withResolver]]. As a Future is completed only once, only one of
    * resolve/reject/complete may be used and only once.
    */
  trait Resolver[-T, Cap^]:
    /** Complete the future with a data item successfully */
    def resolve(item: T): Unit = complete(Success(item))

    /** Complete the future with a failure */
    def reject(exc: Throwable): Unit = complete(Failure(exc))

    /** Complete the future with a [[CancellationException]] */
    def rejectAsCancelled(): Unit = complete(Failure(new CancellationException()))

    /** Complete the future with the result, be it Success or Failure */
    def complete(result: Try[T]): Unit

    /** Register a cancellation handler to be called when the created future is cancelled. Note that only one handler
      * may be used. The handler should eventually complete the Future using one of complete/resolve/reject*. The
      * default handler is set up to [[rejectAsCancelled]] immediately.
      */
    def onCancel(handler: (() -> Unit)^{Cap^}): Unit
  end Resolver

  /** Create a promise that may be completed asynchronously using external means.
    *
    * The body is run synchronously on the callers thread to setup an external asynchronous operation whose
    * success/failure it communicates using the [[Resolver]] to complete the future.
    *
    * If the external operation supports cancellation, the body can register one handler using [[Resolver.onCancel]].
    */
  def withResolver[T, Cap^](body: Resolver[T, Cap]^{Cap^} => Unit): Future[T]^{Cap^} =
    val future: (CoreFuture[T] & Resolver[T, Cap] & Promise[T])^{Cap^} = new CoreFuture[T] with Resolver[T, Cap] with Promise[T]:
      // TODO: undo this once bug is fixed
      @volatile var cancelHandle: (() -> Unit) = () => rejectAsCancelled()
      override def onCancel(handler: (() -> Unit)^{Cap^}): Unit =
        cancelHandle = /* TODO remove */ caps.unsafe.unsafeAssumePure(handler)
      override def complete(result: Try[T]): Unit = super.complete(result)

      override def cancel(): Unit =
        if setCancelled() then cancelHandle()
    end future
    body(future)
    future
  end withResolver

  sealed abstract class BaseCollector[T, Cap^]():
    private val ch = UnboundedChannel[Future[T]^{Cap^}]()

    private val futMap = mutable.Map[SourceSymbol[Try[T]], Future[T]^{Cap^}]()

    /** Output channels of all finished futures. */
    final def results: ReadableChannel[Future[T]^{Cap^}] = ch.asReadable

    private val listener = Listener((_, fut) =>
      // safe, as we only attach this listener to Future[T]
      val future = futMap.synchronized:
        futMap.remove(fut.asInstanceOf[SourceSymbol[Try[T]]]).get
      ch.sendImmediately(future)
    )

    protected final def addFuture(future: Future[T]^{Cap^}) =
      futMap.synchronized { futMap += (future.symbol -> future) }
      future.onComplete(listener)
  end BaseCollector


  /** Collects a list of futures into a channel of futures, arriving as they finish.
    * @example
    *   {{{
    * // Sleep sort
    * val futs = numbers.map(i => Future(sleep(i.millis)))
    * val collector = Collector(futs*)
    *
    * val output = mutable.ArrayBuffer[Int]()
    * for i <- 1 to futs.size:
    *   output += collector.results.read().await
    *   }}}
    * @see
    *   [[Future.awaitAll]] and [[Future.awaitFirst]] for simple usage of the collectors to get all results or the first
    *   succeeding one.
    */
  class Collector[T](futures: (Future[T]^)*) extends BaseCollector[T, caps.CapSet^{futures*}]:
    futures.foreach(addFuture)
  end Collector

  /** Like [[Collector]], but exposes the ability to add futures after creation. */
  class MutableCollector[T, Cap^](futures: (Future[T]^{Cap^})*) extends BaseCollector[T, Cap]:
    futures.foreach(addFuture)
    /** Add a new [[Future]] into the collector. */
    inline def add(future: Future[T]^{Cap^}) = addFuture(future)
    inline def +=(future: Future[T]^{Cap^}) = add(future)

  extension [T](@caps.use fs: Seq[Future[T]^])
    /** `.await` for all futures in the sequence, returns the results in a sequence, or throws if any futures fail. */
    def awaitAll(using Async) =
      val collector = Collector(fs*)
      for _ <- fs do collector.results.read().right.get.await
      fs.map(_.await)

    /** Like [[awaitAll]], but cancels all futures as soon as one of them fails. */
    def awaitAllOrCancel(using Async) =
      val collector = Collector(fs*)
      try
        for _ <- fs do collector.results.read().right.get.await
        fs.map(_.await)
      catch
        case NonFatal(e) =>
          fs.foreach(_.cancel())
          throw e

    /** Race all futures, returning the first successful value. Throws the last exception received, if everything fails.
      */
    def awaitFirst(using Async): T = awaitFirstImpl(false)

    /** Like [[awaitFirst]], but cancels all other futures as soon as the first future succeeds. */
    def awaitFirstWithCancel(using Async): T = awaitFirstImpl(true)

    private inline def awaitFirstImpl(withCancel: Boolean)(using Async): T =
      val collector = Collector(fs*)
      @scala.annotation.tailrec
      def loop(attempt: Int): T =
        collector.results.read().right.get.awaitResult match
          case Failure(exception) =>
            if attempt == fs.length then /* everything failed */ throw exception else loop(attempt + 1)
          case Success(value) =>
            inline if withCancel then fs.foreach(_.cancel())
            value
      loop(1)
end Future

/** TaskSchedule describes the way in which a task should be repeated. Tasks can be set to run for example every 100
  * milliseconds or repeated as long as they fail. `maxRepetitions` describes the maximum amount of repetitions allowed,
  * after that regardless of TaskSchedule chosen, the task is not repeated anymore and the last returned value is
  * returned. `maxRepetitions` equal to zero means that repetitions can go on potentially forever.
  */
enum TaskSchedule:
  case Every(val millis: Long, val maxRepetitions: Long = 0)
  case ExponentialBackoff(val millis: Long, val exponentialBase: Int = 2, val maxRepetitions: Long = 0)
  case FibonacciBackoff(val millis: Long, val maxRepetitions: Long = 0)
  case RepeatUntilFailure(val millis: Long = 0, val maxRepetitions: Long = 0)
  case RepeatUntilSuccess(val millis: Long = 0, val maxRepetitions: Long = 0)

/** A task is a template that can be turned into a runnable future Composing tasks can be referentially transparent.
  * Tasks can be also ran on a specified schedule.
  */
class Task[+T](val body: (Async, AsyncOperations) ?=> T):

  /** Run the current task and returns the result. */
  def run()(using Async, AsyncOperations): T = body

  /** Start a future computed from the `body` of this task */
  def start()(using async: Async, spawn: Async.Spawn)(using asyncOps: AsyncOperations)(using async.type =:= spawn.type): Future[T]^{body, spawn} =
    Future(body)(using async, spawn)

  def schedule(s: TaskSchedule): Task[T]^{body} =
    s match {
      case TaskSchedule.Every(millis, maxRepetitions) =>
        assert(millis >= 1)
        assert(maxRepetitions >= 0)
        Task {
          var repetitions = 0
          var ret: T = body
          repetitions += 1
          if (maxRepetitions == 1) ret
          else {
            while (maxRepetitions == 0 || repetitions < maxRepetitions) {
              AsyncOperations.sleep(millis)
              ret = body
              repetitions += 1
            }
            ret
          }
        }
      case TaskSchedule.ExponentialBackoff(millis, exponentialBase, maxRepetitions) =>
        assert(millis >= 1)
        assert(exponentialBase >= 2)
        assert(maxRepetitions >= 0)
        Task {
          var repetitions = 0
          var ret: T = body
          repetitions += 1
          if (maxRepetitions == 1) ret
          else {
            var timeToSleep = millis
            while (maxRepetitions == 0 || repetitions < maxRepetitions) {
              AsyncOperations.sleep(timeToSleep)
              timeToSleep *= exponentialBase
              ret = body
              repetitions += 1
            }
            ret
          }
        }
      case TaskSchedule.FibonacciBackoff(millis, maxRepetitions) =>
        assert(millis >= 1)
        assert(maxRepetitions >= 0)
        Task {
          var repetitions = 0
          var a: Long = 0
          var b: Long = 1
          var ret: T = body
          repetitions += 1
          if (maxRepetitions == 1) ret
          else {
            AsyncOperations.sleep(millis)
            ret = body
            repetitions += 1
            if (maxRepetitions == 2) ret
            else {
              while (maxRepetitions == 0 || repetitions < maxRepetitions) {
                val aOld = a
                a = b
                b = aOld + b
                AsyncOperations.sleep(b * millis)
                ret = body
                repetitions += 1
              }
              ret
            }
          }
        }
      case TaskSchedule.RepeatUntilFailure(millis, maxRepetitions) =>
        assert(millis >= 0)
        assert(maxRepetitions >= 0)
        Task {
          @tailrec
          def helper(repetitions: Long = 0): T =
            if (repetitions > 0 && millis > 0)
              AsyncOperations.sleep(millis)
            val ret: T = body
            ret match {
              case Failure(_)                                                      => ret
              case _ if (repetitions + 1) == maxRepetitions && maxRepetitions != 0 => ret
              case _                                                               => helper(repetitions + 2)
            }
          helper()
        }
      case TaskSchedule.RepeatUntilSuccess(millis, maxRepetitions) =>
        assert(millis >= 0)
        assert(maxRepetitions >= 0)
        Task {
          @tailrec
          def helper(repetitions: Long = 0): T =
            if (repetitions > 0 && millis > 0)
              AsyncOperations.sleep(millis)
            val ret: T = body
            ret match {
              case Success(_)                                                      => ret
              case _ if (repetitions + 1) == maxRepetitions && maxRepetitions != 0 => ret
              case _                                                               => helper(repetitions + 2)
            }
          helper()
        }
    }

end Task

/** Runs the `body` inside in an [[Async]] context that does *not* propagate cancellation until the end.
  *
  * In other words, `body` is never notified of the cancellation of the `ac` context; but `uninterruptible` would still
  * throw a [[CancellationException]] ''after `body` finishes running'' if `ac` was cancelled.
  */
def uninterruptible[T](body: Async ?=> T)(using ac: Async): T =
  val tracker = Cancellable.Tracking().link()

  val r =
    try
      val group = CompletionGroup()
      Async.withNewCompletionGroup(group)(body)
    finally tracker.unlink()

  if tracker.isCancelled then throw new CancellationException()
  r

/** Link `cancellable` to the completion group of the current [[Async]] context during `fn`.
  *
  * If the [[Async]] context is cancelled during the execution of `fn`, `cancellable` will also be immediately
  * cancelled.
  */
def cancellationScope[T](cancellable: Cancellable)(fn: => T)(using a: Async): T =
  cancellable.link()
  try fn
  finally cancellable.unlink()
