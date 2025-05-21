package dotty.tools.pc

import java.util.concurrent.ScheduledExecutorService
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.internal.metals.Report
import java.util.logging.Logger
import java.util.logging.Level
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.ScheduledFuture

case class InfiniteCompilationException(msg: String) extends Exception // we should expose this in mtags-interfaces so we can catch it in IDE's

class Worker(queue: TaskQueue, config: PresentationCompilerConfig, reportContext: PcReportContext):
  private val logger: Logger = Logger.getLogger(getClass.getName).nn

  private val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor().nn
  private val timeoutExecutor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor().nn
  private var _currentTask: AtomicReference[Option[queue.Task[?]]] = AtomicReference(None)

  def scheduleProcessing(): Unit =
    executor.execute(() => processQueue())

  private def handleInfiniteCompilation(thread: Thread, task: queue.Task[?]): Unit =
    val compilationInputs = task.inputs
    val stacktrace =
      thread.getStackTrace().nn
        .map(_.nn.toString())
        .mkString("\n")

    val shortMessage =
        """Fatal compiler error encountered.
          |Please send this report to compiler team or create an issue at https://github.com/scala/scala3/issues""".stripMargin

    val report =
      Report(
        "fatal-compiler-error",
        s"""|occurred in the presentation compiler.
            |
            |There is very high chance that you've just discovered infinite compilation.
            |Please report this fatal error to the compiler team by uploading this report.
            |
            |You can do at https://github.com/scala/scala3/issues
            |
            |If your code is sensitive please make sure to remove code from this report.
            |
            |Additional data:
            |  ${reportContext.additionalData}
            |Stacktrace:
            |  ${stacktrace}
            |
            |action parameters:
            |${compilationInputs.show}
            |""".stripMargin,
        shortMessage,
        path = Some(compilationInputs.uri)
      )

    reportContext.unsanitized.create(report) match
      case Some(path) =>
        logger.log(Level.SEVERE, s"$shortMessage. Full details of the error can be found in the error report $path")
      case _ =>
        logger.log(Level.SEVERE, shortMessage + "\n" + stacktrace.indent(2))
    task.future.completeExceptionally(new InfiniteCompilationException(shortMessage))

  private def scheduleTaskTimeout(task: queue.Task[?]): ScheduledFuture[Unit] =
    timeoutExecutor.schedule[Unit](() => {
      if !task.future.isDone() then
        logger.log(Level.WARNING, s"Task timeout detected. Attempting graceful cancellation.")
        task.interrupt() // Attempt graceful cancellation
    }, config.timeoutDelay(), config.timeoutUnit()).nn

  private def scheduleForcedShutdown(executorThread: Thread, task: queue.Task[?]): ScheduledFuture[Unit] =
    timeoutExecutor.schedule[Unit](() => {
      logger.log(Level.SEVERE, s"Task did not respond to cancellation after 10 seconds. Forcing executor shutdown.")
      executor.shutdown()
      try
        if !executor.awaitTermination(5, TimeUnit.SECONDS) then
          executor.shutdownNow()
      catch
        case _: InterruptedException => executor.shutdownNow()
      handleInfiniteCompilation(executorThread, task)
      ()
    }, 60, TimeUnit.SECONDS).nn // refactor and use value from config

  private def processQueue(): Unit =
    try
      val thisThread = Thread.currentThread().nn
      queue.dequeue().foreach: task =>
        _currentTask.set(Some(task))

        val initialTimeoutFuture = scheduleTaskTimeout(task)
        val forceShutdownFuture  = scheduleForcedShutdown(thisThread, task)

        task.execute() // actual compilation happens here

        initialTimeoutFuture.cancel(false)
        forceShutdownFuture.cancel(false)

        processQueue()
    finally
      _currentTask.getAndSet(None).nn.foreach(_.interrupt())

  def shutdown() =
    executor.shutdown()
    timeoutExecutor.shutdown()
