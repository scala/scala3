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

  /** A marker type for Source#CanFilter */
  opaque type Yes = Unit

  abstract class Impl(val root: Cancellable, val scheduler: Scheduler)
     (using boundary.Label[Unit]) extends Async:

    protected def checkCancellation(): Unit

    def await[T](src: Async.Source[T]): T =
      checkCancellation()
      var resultOpt: Option[T] = None
      src.poll: x =>
        resultOpt = Some(x)
        true
      resultOpt.getOrElse:
        try suspend[T, Unit]: k =>
          src.onComplete: x =>
            scheduler.schedule: () =>
              k.resume(x)
            true // signals to `src` that result `x` was consumed
        finally checkCancellation()

  end Impl

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

  /** An asynchronous data source. Sources can be persistent or ephemeral.
   *  A persistent source will always pass same data to calls of `poll and `onComplete`.
   *  An ephememral source can pass new data in every call.
   *  An example of a persistent source is `Future`.
   *  An example of an ephemeral source is `Channel`.
   */
  trait Source[+T]:

    type CanFilter

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

  end Source

  /** As source that transforms an original source in some way */

  abstract class DerivedSource[T, U](val original: Source[T]) extends Source[U]:

    /** Handle a value `x` passed to the original source by possibly
     *  invokiong the continuation for this source.
     */
    protected def listen(x: T, k: Listener[U]): Boolean

    private def transform(k: Listener[U]): Listener[T] =
      new ForwardingListener[T](this, k):
        def apply(x: T): Boolean = listen(x, k)

    def poll(k: Listener[U]): Boolean =
      original.poll(transform(k))
    def onComplete(k: Listener[U]): Unit =
      original.onComplete(transform(k))
    def dropListener(k: Listener[U]): Unit =
      original.dropListener(transform(k))
  end DerivedSource

  extension [T](src: Source[T])

    /** Pass on data transformed by `f` */
    def map[U](f: T => U): Source[U] { type CanFilter = src.CanFilter } =
      new DerivedSource[T, U](src):
        type CanFilter = src.CanFilter
        def listen(x: T, k: Listener[U]) = k(f(x))

  extension [T](src: Source[T] { type CanFilter = Yes })

    /** Pass on only data matching the predicate `p` */
    def filter(p: T => Boolean): Source[T] { type CanFilter = src.CanFilter } =
      new DerivedSource[T, T](src):
        type CanFilter = src.CanFilter
        def listen(x: T, k: Listener[T]) = p(x) && k(x)


  /** Pass first result from any of `sources` to the continuation */
  def race[T, CF](sources: Source[T] { type CanFilter <: CF} *): Source[T] { type CanFilter <: CF } =
    new Source[T]:
      type CanFilter <: CF

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
  def either[T, U, CF](
      src1: Source[T] { type CanFilter <: CF },
      src2: Source[U] { type CanFilter <: CF })
    : Source[Either[T, U]] { type CanFilter <: CF } =
    race[Either[T, U], CF](src1.map(Left(_)), src2.map(Right(_)))

end Async

