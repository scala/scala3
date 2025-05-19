package dotty.tools.pc


import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams
import java.util.concurrent.CompletableFuture
import scala.meta.internal.metals.EmptyCancelToken
import java.net.URI
import scala.meta.pc.OffsetParams

case class CompilationInputs(
  uri: URI,
  code: String,
  cancelToken: CancelToken = EmptyCancelToken,
  cleanDriver: Boolean = false
):
  def show: String =
    s"""|uri: $uri
        |code: $code
        |cancelToken: $cancelToken
        |cleanDriver: $cleanDriver
        |""".stripMargin

object CompilationInputs:
  def empty: CompilationInputs = CompilationInputs(new URI(""), "", EmptyCancelToken, false)
  def fromParams(params: VirtualFileParams | OffsetParams, cleanDriver: Boolean = false): CompilationInputs =
    CompilationInputs(params.uri().nn, params.text().nn, params.token().nn, cleanDriver)

class DriverAccess(
  config: PresentationCompilerConfig,
  driverSettings: List[String],
  reportContext: PcReportContext
):

  private val taskQueue = TaskQueue(driverSettings, reportContext)
  private val worker = Worker(taskQueue, config, reportContext)

  def lookup[T](f: InteractiveDriver => T): CompletableFuture[T] =
    enqueueTask(taskQueue.LookupTask(f))

  def enqueueCancellable[T](inputs: CompilationInputs)
    (f: CancelToken ?=> InteractiveDriver => T): CompletableFuture[T] =

    given token: CancelToken = inputs.cancelToken
    val task = taskQueue.CompilationTask(token, inputs)(f)

    enqueueTask(task)

  end enqueueCancellable

  private def enqueueTask[T](task: taskQueue.Task[T]): CompletableFuture[T] =
    taskQueue.enqueue(task)
    worker.scheduleProcessing()
    task.future

  def shutdown(): Unit =
    worker.shutdown()
    taskQueue.shutdown()

  def restart() =
    taskQueue.shutdown()

