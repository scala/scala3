package gears.async

import language.experimental.captureChecking

import scala.concurrent.ExecutionContext
import scala.concurrent.{Future as StdFuture, Promise as StdPromise}
import scala.util.Try

/** Converters from Gears types to Scala API types and back. */
object ScalaConverters:
  extension [T](fut: StdFuture[T]^)
    /** Converts a [[scala.concurrent.Future Scala Future]] into a gears [[Future]]. Requires an
      * [[scala.concurrent.ExecutionContext ExecutionContext]], as the job of completing the returned [[Future]] will be
      * done through this context. Since [[scala.concurrent.Future Scala Future]] cannot be cancelled, the returned
      * [[Future]] will *not* clean up the pending job when cancelled.
      */
    def asGears(using ExecutionContext): Future[T]^{fut} =
      Future.withResolver[T, caps.CapSet]: resolver =>
        fut.andThen(result => resolver.complete(result))

  extension [T](fut: Future[T]^)
    /** Converts a gears [[Future]] into a Scala [[scala.concurrent.Future Scala Future]]. Note that if `fut` is
      * cancelled, the returned [[scala.concurrent.Future Scala Future]] will also be completed with
      * `Failure(CancellationException)`.
      */
    def asScala: StdFuture[T]^{fut} =
      val p = StdPromise[T]()
      fut.onComplete(Listener((res, _) => p.complete(res)))
      p.future
