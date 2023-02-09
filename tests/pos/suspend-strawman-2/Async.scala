package concurrent
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import runtime.suspend
import scala.util.boundary

/** The underlying configuration of an async block */
trait AsyncConfig:

  /** The cancellable async source underlying this async computation */
  def root: Cancellable

  /** The scheduler for runnables defined in this async computation */
  def scheduler: Scheduler

object AsyncConfig:

  /** A toplevel async group with given scheduler and a synthetic root that
   *  ignores cancellation requests
   */
  given fromScheduler(using s: Scheduler): AsyncConfig with
    def root = Cancellable.empty
    def scheduler = s

end AsyncConfig

/** A context that allows to suspend waiting for asynchronous data sources */
trait Async extends AsyncConfig:

  /** Wait for completion of async source `src` and return the result */
  def await[T](src: Async.Source[T]): T

object Async:

  abstract class AsyncImpl(val root: Cancellable, val scheduler: Scheduler)
     (using boundary.Label[Unit]) extends Async:

    protected def checkCancellation(): Unit

    private var result: T

    def await[T](src: Async.Source[T]): T =
      checkCancellation()
      var resultOpt: Option[T] = None
      if src.poll: x =>
        result = x
        true
      then result
      else
        try suspend[T, Unit]: k =>
          src.onComplete: x =>
            scheduler.schedule: () =>
              k.resume(x)
            true // signals to `src` that result `x` was consumed
        finally checkCancellation()

  end AsyncImpl

  /** The currently executing Async context */
  inline def current(using async: Async): Async = async

  /** Await source result in currently executing Async context */
  inline def await[T](src: Source[T])(using async: Async): T = async.await(src)

  /** A function `T => Boolean` whose lineage is recorded by its implementing
   *  classes. The Listener function accepts values of type `T` and returns
   *  `true` iff the value was consumed by an async block.
   */
  trait Listener[-T] extends Function[T, Boolean]

  /** A listener for values that are processed by the given source `src` and
   *  that are demanded by the continuation listener `continue`.
   */
  abstract case class ForwardingListener[T](src: Source[?], continue: Listener[?]) extends Listener[T]

  /** A listener for values that are processed directly in an async block.
   *  Closures of type `T => Boolean` can be SAM converted to this type.
   */
  abstract case class FinalListener[T]() extends Listener[T]

  /** A source that cannot be mapped, filtered, or raced. In other words,
   *  an item coming from a direct source must be immediately consumed in
   *  another async computation; no rejection of this item is possible.
   */
  trait DirectSource[+T]:

    /** If data is available at present, pass it to function `k`
     *  and return the result if this call.
     *  `k` returns true iff the data was consumed in an async block.
     *  Calls to `poll` are always synchronous.
     */
    def poll(k: Listener[T]): Boolean

    /** Once data is available, pass it to function `k`.
     *  `k` returns true iff the data was consumed in an async block.
     *  Calls to `onComplete` are usually asynchronous, meaning that
     *  the passed continuation `k` is a suspension.
     */
    def onComplete(k: Listener[T]): Unit

    /** Signal that listener `k` is dead (i.e. will always return `false` from now on).
     *  This permits original, (i.e. non-derived) sources like futures or channels
     *  to drop the  listener from their `waiting` sets.
     */
    def dropListener(k: Listener[T]): Unit

  end DirectSource

  /** An asynchronous data source. Sources can be persistent or ephemeral.
   *  A persistent source will always pass same data to calls of `poll and `onComplete`.
   *  An ephememral source can pass new data in every call.
   *  An example of a persistent source is `Future`.
   *  An example of an ephemeral source is `Channel`.
   */
  trait Source[+T] extends DirectSource[T]:

    /** Pass on data transformed by `f` */
    def map[U](f: T => U): Source[U] =
      new DerivedSource[T, U](this):
        def listen(x: T, k: Listener[U]) = k(f(x))

    /** Pass on only data matching the predicate `p` */
    def filter(p: T => Boolean): Source[T] =
      new DerivedSource[T, T](this):
        def listen(x: T, k: Listener[T]) = p(x) && k(x)

  end Source

  /** As source that transforms an original source in some way */

  abstract class DerivedSource[T, U](src: Source[T]) extends Source[U]:

    /** Handle a value `x` passed to the original source by possibly
     *  invokiong the continuation for this source.
     */
    protected def listen(x: T, k: Listener[U]): Boolean

    private def transform(k: Listener[U]): Listener[T] =
      new ForwardingListener[T](this, k):
        def apply(x: T): Boolean = listen(x, k)

    def poll(k: Listener[U]): Boolean =
      src.poll(transform(k))
    def onComplete(k: Listener[U]): Unit =
      src.onComplete(transform(k))
    def dropListener(k: Listener[U]): Unit =
      src.dropListener(transform(k))
  end DerivedSource

  /** Pass first result from any of `sources` to the continuation */
  def race[T](sources: Source[T]*): Source[T] = new Source:

    def poll(k: Listener[T]): Boolean =
      val it = sources.iterator
      var found = false
      while it.hasNext && !found do
        it.next.poll: x =>
          found = k(x)
          found
      found

    def onComplete(k: Listener[T]): Unit =
      val listener = new ForwardingListener[T](this, k):
        var foundBefore = false
        def continueIfFirst(x: T): Boolean = synchronized:
          if foundBefore then false else { foundBefore = k(x); foundBefore }
        def apply(x: T): Boolean =
          val found = continueIfFirst(x)
          if found then sources.foreach(_.dropListener(this))
          found
      sources.foreach(_.onComplete(listener))

    def dropListener(k: Listener[T]): Unit =
      val listener = new ForwardingListener[T](this, k):
        def apply(x: T): Boolean = ???
          // not to be called, we need the listener only for its
          // hashcode and equality test.
      sources.foreach(_.dropListener(listener))

  end race

  /** If left (respectively, right) source succeeds with `x`, pass `Left(x)`,
   *  (respectively, Right(x)) on to the continuation.
   */
  def either[T, U](src1: Source[T], src2: Source[U]): Source[Either[T, U]] =
    race[Either[T, U]](src1.map(Left(_)), src2.map(Right(_)))

end Async

