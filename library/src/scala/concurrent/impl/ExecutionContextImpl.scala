/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.impl

import scala.language.`2.13`
import java.util.concurrent.{ Semaphore, ForkJoinPool, ForkJoinWorkerThread, Callable, Executor, ExecutorService, ThreadFactory, TimeUnit }
import java.util.Collection
import scala.concurrent.{ BlockContext, ExecutionContext, CanAwait, ExecutionContextExecutor, ExecutionContextExecutorService }

private[scala] class ExecutionContextImpl private[impl] (final val executor: Executor, final val reporter: Throwable => Unit) extends ExecutionContextExecutor {
  require(executor ne null, "Executor must not be null")
  /** Executes the given runnable task using the underlying executor.
   *
   *  @param runnable the task to execute
   */
  override final def execute(runnable: Runnable): Unit = executor.execute(runnable)
  /** Reports the given throwable to the configured reporter.
   *
   *  @param t the throwable to report
   */
  override final def reportFailure(t: Throwable): Unit = reporter(t)
}

private[concurrent] object ExecutionContextImpl {

  /** A thread factory that creates daemon or non-daemon threads with configurable
   *  blocking behavior and exception handling.
   *
   *  @param daemonic whether created threads should be daemon threads
   *  @param maxBlockers the maximum number of threads that can block simultaneously
   *  @param prefix the prefix to use for thread names
   *  @param uncaught the handler for uncaught exceptions in created threads
   */
  final class DefaultThreadFactory(
    /** Whether created threads should be daemon threads */
    final val daemonic: Boolean,
    /** The maximum number of threads that can block simultaneously */
    final val maxBlockers: Int,
    /** The prefix to use for thread names */
    final val prefix: String,
    /** The handler for uncaught exceptions in created threads */
    final val uncaught: Thread.UncaughtExceptionHandler) extends ThreadFactory with ForkJoinPool.ForkJoinWorkerThreadFactory {

    require(prefix ne null, "DefaultThreadFactory.prefix must be non null")
    require(maxBlockers >= 0, "DefaultThreadFactory.maxBlockers must be greater-or-equal-to 0")

    private final val blockerPermits = new Semaphore(maxBlockers)

    /** Configures the given thread with daemon status, exception handler, and name.
     *
     *  @tparam T the type of thread to configure
     *  @param thread the thread to configure
     *  @return the configured thread
     */
    @annotation.nowarn("cat=deprecation")
    def wire[T <: Thread](thread: T): T = {
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler(uncaught)
      thread.setName(prefix + "-" + thread.getId())
      thread
    }

    /** Creates a new thread that will execute the given runnable.
     *
     *  @param runnable the task the thread will execute
     *  @return the newly created thread
     */
    def newThread(runnable: Runnable): Thread = wire(new Thread(runnable))

    /** Creates a new ForkJoinWorkerThread that supports blocking operations.
     *
     *  @param fjp the ForkJoinPool this thread will belong to
     *  @return the newly created ForkJoinWorkerThread
     */
    def newThread(fjp: ForkJoinPool): ForkJoinWorkerThread =
      wire(new ForkJoinWorkerThread(fjp) with BlockContext {
        private final var isBlocked: Boolean = false // This is only ever read & written if this thread is the current thread
        final override def blockOn[T](thunk: => T)(implicit permission: CanAwait): T =
          if ((Thread.currentThread eq this) && !isBlocked && blockerPermits.tryAcquire()) {
            try {
              val b: (ForkJoinPool.ManagedBlocker & (() => T)) =
                new ForkJoinPool.ManagedBlocker with (() => T) {
                  private final var result: T = null.asInstanceOf[T]
                  private final var done: Boolean = false
                  final override def block(): Boolean = {
                    if (!done) {
                      result = thunk // If this throws then it will stop blocking.
                      done = true
                    }

                    isReleasable
                  }

                  final override def isReleasable = done
                  final override def apply(): T = result
                }
              isBlocked = true
              ForkJoinPool.managedBlock(b)
              b()
            } finally {
              isBlocked = false
              blockerPermits.release()
            }
          } else thunk // Unmanaged blocking
      })
  }

  /** Creates a default ForkJoinPool-based ExecutionContextExecutorService.
   *
   *  @param reporter the function to report uncaught exceptions
   *  @return a new ExecutionContextExecutorService
   */
  def createDefaultExecutorService(reporter: Throwable => Unit): ExecutionContextExecutorService = {
    def getInt(name: String, default: String) = (try System.getProperty(name, default) catch {
      case e: SecurityException => default
    }) match {
      case s if s.charAt(0) == 'x' => (Runtime.getRuntime.availableProcessors * s.substring(1).toDouble).ceil.toInt
      case other => other.toInt
    }

    val desiredParallelism = // A range between min and max given num
      scala.math.min(
        scala.math.max(
          getInt("scala.concurrent.context.minThreads", "1"),
          getInt("scala.concurrent.context.numThreads", "x1")),
          getInt("scala.concurrent.context.maxThreads", "x1")
        )

    val threadFactory = new DefaultThreadFactory(daemonic = true,
                                                 maxBlockers = getInt("scala.concurrent.context.maxExtraThreads", "256"),
                                                 prefix = "scala-execution-context-global",
                                                 uncaught = (thread: Thread, cause: Throwable) => reporter(cause))

    new ForkJoinPool(desiredParallelism, threadFactory, threadFactory.uncaught, true) with ExecutionContextExecutorService {
      final override def reportFailure(cause: Throwable): Unit =
        getUncaughtExceptionHandler() match {
          case null =>
          case some => some.uncaughtException(Thread.currentThread, cause)
        }
    }
  }

  /** Creates an ExecutionContextExecutor from the given executor.
   *
   *  @param e the executor to wrap, or null to use the default
   *  @param reporter the function to report uncaught exceptions
   *  @return a new ExecutionContextExecutor
   */
  def fromExecutor(e: Executor | Null, reporter: Throwable => Unit = ExecutionContext.defaultReporter): ExecutionContextExecutor =
    e match {
      case null => createDefaultExecutorService(reporter)
      case some => new ExecutionContextImpl(some, reporter)
    }

  /** Creates an ExecutionContextExecutorService from the given executor service.
   *
   *  @param es the executor service to wrap, or null to use the default
   *  @param reporter the function to report uncaught exceptions
   *  @return a new ExecutionContextExecutorService
   */
  def fromExecutorService(es: ExecutorService | Null, reporter: Throwable => Unit = ExecutionContext.defaultReporter):
    ExecutionContextExecutorService = es match {
      case null => createDefaultExecutorService(reporter)
      case some =>
        // This is a anonymous class extending a Java class, so we left inferred flexible types in the signatures.
        new ExecutionContextImpl(some, reporter) with ExecutionContextExecutorService {
            private final def asExecutorService: ExecutorService = executor.asInstanceOf[ExecutorService]
            final override def shutdown() = asExecutorService.shutdown()
            final override def shutdownNow(): java.util.List[Runnable] = asExecutorService.shutdownNow()
            final override def isShutdown = asExecutorService.isShutdown
            final override def isTerminated = asExecutorService.isTerminated
            final override def awaitTermination(l: Long, timeUnit: TimeUnit) = asExecutorService.awaitTermination(l, timeUnit)
            final override def submit[T](callable: Callable[T]): java.util.concurrent.Future[T] = asExecutorService.submit(callable)
            final override def submit[T](runnable: Runnable, t: T): java.util.concurrent.Future[T] = asExecutorService.submit(runnable, t)
            final override def submit(runnable: Runnable): java.util.concurrent.Future[?] = asExecutorService.submit(runnable)
            final override def invokeAll[T](callables: Collection[? <: Callable[T]]): java.util.List[java.util.concurrent.Future[T]] = asExecutorService.invokeAll(callables)
            final override def invokeAll[T](callables: Collection[? <: Callable[T]], l: Long, timeUnit: TimeUnit): java.util.List[java.util.concurrent.Future[T]] = asExecutorService.invokeAll(callables, l, timeUnit)
            final override def invokeAny[T](callables: Collection[? <: Callable[T]]): T = asExecutorService.invokeAny(callables)
            final override def invokeAny[T](callables: Collection[? <: Callable[T]], l: Long, timeUnit: TimeUnit): T = asExecutorService.invokeAny(callables, l, timeUnit)
          }
        }
}
