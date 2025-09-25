package gears.async

import language.experimental.captureChecking

object JvmAsyncOperations extends AsyncOperations:
  override def sleep(millis: Long)(using Async): Unit =
    jvmInterruptible(Thread.sleep(millis))

  /** Runs `fn` in a [[cancellationScope]] where it will be interrupted (as a Java thread) upon cancellation.
    *
    * Note that `fn` will need to handle both [[java.util.concurrent.CancellationException]] (when performing Gears
    * operations such as `.await`) *and* [[java.lang.InterruptedException]], so the intended use case is usually to wrap
    * interruptible Java operations, containing `fn` to a narrow scope.
    */
  def jvmInterruptible[T](fn: => T)(using Async): T =
    val th = Thread.currentThread()
    cancellationScope(() => th.interrupt()):
      try fn
      catch case _: InterruptedException => throw new CancellationException()
