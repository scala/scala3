package dotty.tools.backend.jvm

import java.nio.channels.ClosedByInterruptException
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent.*

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.profile.ThreadPoolFactory
import scala.util.control.NonFatal
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Decorators.em

import scala.compiletime.uninitialized

/**
 * Interface to handle post-processing and classfile writing (see [[PostProcessor]]) of generated
 * classes, potentially in parallel.
 */
private[jvm] sealed trait GeneratedClassHandler {
  val postProcessor: PostProcessor

  /**
    * Pass the result of code generation for a compilation unit to this handler for post-processing
    */
  def process(unit: GeneratedCompilationUnit): Unit

  /**
   * If running in parallel, block until all generated classes are handled
   */
  def complete(): Unit

  /**
    * Invoked at the end of the jvm phase
    */
  def close(): Unit = ()
}

private[jvm] object GeneratedClassHandler {
  def apply(postProcessor: PostProcessor)(using ictx: Context): GeneratedClassHandler = {
    val compilerSettings = postProcessor.frontendAccess.compilerSettings
    val handler = compilerSettings.backendParallelism match {
      case 1 => new SyncWritingClassHandler(postProcessor)

      case maxThreads =>
        // if (settings.areStatisticsEnabled)
        //   runReporting.warning(
        //     NoPosition,
        //     "JVM statistics are not reliable with multi-threaded JVM class writing.\n" +
        //     "To collect compiler statistics remove the " + settings.YaddBackendThreads.name + " setting.",
        //     WarningCategory.Other,
        //     site = ""
        //   )
        val additionalThreads = maxThreads - 1
        // The thread pool queue is limited in size. When it's full, the `CallerRunsPolicy` causes
        // a new task to be executed on the main thread, which provides back-pressure.
        // The queue size is large enough to ensure that running a task on the main thread does
        // not take longer than to exhaust the queue for the backend workers.
        val queueSize = compilerSettings.backendMaxWorkerQueue.getOrElse(maxThreads * 2)
        val threadPoolFactory = ThreadPoolFactory(Phases.genBCodePhase)
        val javaExecutor = threadPoolFactory.newBoundedQueueFixedThreadPool(additionalThreads, queueSize, new CallerRunsPolicy, "non-ast")
        new AsyncWritingClassHandler(postProcessor, javaExecutor)
    }

    // if (settings.optInlinerEnabled || settings.optClosureInvocations) new GlobalOptimisingGeneratedClassHandler(postProcessor, handler)
    // else
    handler
  }

  sealed abstract class WritingClassHandler(val javaExecutor: Executor) extends GeneratedClassHandler {
    import postProcessor.frontendAccess

    def tryStealing: Option[Runnable]

    private val processingUnits = ListBuffer.empty[CompilationUnitInPostProcess]

    def process(unit: GeneratedCompilationUnit): Unit = {
      val unitInPostProcess = new CompilationUnitInPostProcess(unit.classes, unit.tasty, unit.sourceFile)(using unit.ctx)
      postProcessUnit(unitInPostProcess)
      processingUnits += unitInPostProcess
    }

    protected implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(javaExecutor)

    final def postProcessUnit(unitInPostProcess: CompilationUnitInPostProcess): Unit = {
      unitInPostProcess.task = Future:
        frontendAccess.withThreadLocalReporter(unitInPostProcess.bufferedReporting):
          // we 'take' classes to reduce the memory pressure
          // as soon as the class is consumed and written, we release its data
          unitInPostProcess.takeClasses().foreach:
            postProcessor.sendToDisk(_, unitInPostProcess.sourceFile)
          unitInPostProcess.takeTasty().foreach:
            postProcessor.sendToDisk(_, unitInPostProcess.sourceFile)
    }

    protected def takeProcessingUnits(): List[CompilationUnitInPostProcess] = {
      val result = processingUnits.result()
      processingUnits.clear()
      result
    }

    final def complete(): Unit = {
      import frontendAccess.directBackendReporting

      def stealWhileWaiting(unitInPostProcess: CompilationUnitInPostProcess): Unit = {
        val task = unitInPostProcess.task
        while (!task.isCompleted)
          tryStealing match {
            case Some(r) => r.run()
            case None => Await.ready(task, Duration.Inf)
          }
      }

      /*
       * Go through each task in submission order, wait for it to finish and report its messages.
       * When finding task that has not completed, steal work from the executor's queue and run
       * it on the main thread (which we are on here), until the task is done.
       *
       * We could consume the results when they are ready, via use of a [[java.util.concurrent.CompletionService]]
       * or something similar, but that would lead to non deterministic reports from backend threads, as the
       * compilation unit could complete in a different order than when they were submitted, and thus the relayed
       * reports would be in a different order.
       * To avoid that non-determinism we read the result in order of submission, with a potential minimal performance
       * loss, due to the memory being retained longer for tasks than it might otherwise.
       * Most of the memory in the CompilationUnitInPostProcess is reclaimable anyway as the classes are dereferenced after use.
       */
      takeProcessingUnits().foreach { unitInPostProcess =>
        try
          stealWhileWaiting(unitInPostProcess)
          unitInPostProcess.bufferedReporting.relayReports(directBackendReporting)
          // We know the future is complete, throw the exception if it completed with a failure
          unitInPostProcess.task.value.get.get
        catch
          case _: ClosedByInterruptException => throw new InterruptedException()
          case NonFatal(t) =>
            t.printStackTrace()
            frontendAccess.backendReporting.error(em"unable to write ${unitInPostProcess.sourceFile} $t")
      }
    }
  }

  private final class SyncWritingClassHandler(val postProcessor: PostProcessor)
    extends WritingClassHandler(_.nn.run()) {

    override def toString: String = "SyncWriting"

    def tryStealing: Option[Runnable] = None
  }

  private final class AsyncWritingClassHandler(val postProcessor: PostProcessor, override val javaExecutor: ThreadPoolExecutor)
    extends WritingClassHandler(javaExecutor) {

    override def toString: String = s"AsyncWriting[additional threads:${javaExecutor.getMaximumPoolSize}]"

    override def close(): Unit = {
      super.close()
      javaExecutor.shutdownNow()
    }

    def tryStealing: Option[Runnable] = Option(javaExecutor.getQueue.poll())
  }

}

/**
 * State for a compilation unit being post-processed.
 *   - Holds the classes to post-process (released for GC when no longer used)
 *   - Keeps a reference to the future that runs the post-processor
 *   - Buffers messages reported during post-processing
 */
final private class CompilationUnitInPostProcess(private var classes: List[GeneratedClass], private var tasty: List[GeneratedTasty], val sourceFile: AbstractFile)(using Context) {
  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }

  def takeTasty(): List[GeneratedTasty] = {
    val v = tasty
    tasty = Nil
    v
  }

  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = uninitialized

  val bufferedReporting = new BufferingBackendReporting()
}
