package dotty.tools.pc

import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.pc.CancelToken
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import scala.meta.internal.pc.CompilerThrowable
import scala.meta.internal.metals.Report
import java.util.logging.Logger
import java.util.logging.Level
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CancellationException
import java.util.concurrent.ConcurrentLinkedQueue
import dotty.tools.dotc.sbt.interfaces.ProgressCallback

class TaskQueue(driverSettings: List[String], reportContext: PcReportContext):
  private val logger: Logger = Logger.getLogger(getClass.getName).nn

  private val queue: ConcurrentLinkedQueue[Task[?]] = ConcurrentLinkedQueue[Task[?]]() // Not started tasks only
  private val _driver: AtomicReference[CachingDriver] = AtomicReference(newCompiler())

  private def newCompiler(): CachingDriver = new CachingDriver(driverSettings)

  def enqueue(task: Task[?]): Boolean = queue.add(task)
  def dequeue(): Option[Task[?]] = Option(queue.poll().nn)

  def shutdownCurrentCompiler(): Unit =
    _driver.set(new CachingDriver(driverSettings))

  private def getDriver(cleanDriver: Boolean): CachingDriver =
    if cleanDriver then new CachingDriver(driverSettings)
    else _driver.get().nn

  sealed trait Task[T]:
    private val _interrupted = AtomicBoolean(false)

    val inputs: CompilationInputs
    val future: CompletableFuture[T] = new CompletableFuture()

    def execute(): Unit

    final def interrupt(): Unit = _interrupted.set(true)
    final def isInterrupted: Boolean = _interrupted.get()

  case class CompilationTask[T](token: CancelToken, val inputs: CompilationInputs)(f: InteractiveDriver => T) extends Task[T]:

    val callback = new ProgressCallback:
      override def isCancelled(): Boolean = token.nn.isCanceled() || isInterrupted || Thread.interrupted()

    private def compile(): T =
      import inputs.*
      val driver = getDriver(cleanDriver)
      driver.runWithProgressCallback(uri, code, callback)
      f(driver)

    override def execute(): Unit = this.synchronized:
      try
        if callback.isCancelled then future.cancel(true)
        else future.complete(compile())
      catch case ex: Exception =>
        // ProgressCallback.isCancelled = true throws random errors in the compiler, lets return CancellationException instead
        if token.nn.isCanceled then future.completeExceptionally(new CancellationException())
        else if isInterrupted  then future.completeExceptionally(new InterruptedException())
        else
          handleError(ex, (inputs))
          future.completeExceptionally(ex)

  case class LookupTask[T](f: InteractiveDriver => T) extends Task[T]:
    override val inputs: CompilationInputs = CompilationInputs.empty

    private def compile(): T =
      val driver = _driver.get().nn
      f(driver)

    override def execute(): Unit =
      try
        future.complete(compile())
      catch case ex: Exception =>
        future.completeExceptionally(ex)

  private def handleError(e: Throwable, compilationInputs: CompilationInputs): Unit =
    val error = CompilerThrowable.trimStackTrace(e)
    val report =
      Report(
        "compiler-error",
        s"""|occurred in the Presentation Compiler.
            |
            |Presentation Compiler configuration:
            |
            |action parameters:
            |${compilationInputs.show}
            |
            |Additional data:
            |  ${reportContext.additionalData}
            |""".stripMargin,
        error,
        path = Some(compilationInputs.uri)
      )

    val pathToReport = reportContext.unsanitized.create(report)
    pathToReport match {
      case Some(path) =>
        logger.log(
          Level.SEVERE,
          s"A severe compiler error occurred, full details of the error can be found in the error report $path"
        )
      case _ =>
        logger.log(Level.SEVERE, error.getMessage, error)
    }

    shutdownCurrentCompiler()

  def shutdown() = shutdownCurrentCompiler()
