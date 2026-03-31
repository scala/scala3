package dotty.tools.pc

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContextExecutor
import scala.meta.internal.pc.CompilerAccess
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.reporting.StoreReporter

class Scala3CompilerAccess(
    config: PresentationCompilerConfig,
    sh: Option[ScheduledExecutorService],
    newCompiler: () => Scala3CompilerWrapper
)(using ec: ExecutionContextExecutor, rc: ReportContext)
    extends CompilerAccess[StoreReporter, InteractiveDriver](
      config,
      sh,
      newCompiler,
      /* If running inside the executor, we need to reset the job queue
       * Otherwise it will block indefinetely in case of infinite loops.
       */
      shouldResetJobQueue = true
    ):

  def newReporter = new StoreReporter(null)

  /** Handle the exception in order to make sure that we retry immediately.
   *  Otherwise, we will wait until the end of the timeout, which is 20s by
   *  default.
   */
  protected def handleSharedCompilerException(
      t: Throwable
  ): Option[String] = None

  protected def ignoreException(t: Throwable): Boolean = false
end Scala3CompilerAccess
