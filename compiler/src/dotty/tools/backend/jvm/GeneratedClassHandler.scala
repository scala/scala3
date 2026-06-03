package dotty.tools.backend.jvm

import java.nio.channels.ClosedByInterruptException
import java.util.concurrent.*
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.profile.{ProfiledThreadPool, Profiler}

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
   * If running in parallel, block until all generated classes are handled.
   * Returns any exceptions encountered during processing, with the corresponding file.
   */
  def complete(): List[(Throwable, AbstractFile)]

  /**
    * Invoked at the end of the jvm phase
    */
  def close(): Unit = ()
}

private[jvm] object GeneratedClassHandler {
  def serial(postProcessor: PostProcessor): GeneratedClassHandler =
    new SyncWritingClassHandler(postProcessor)

  def parallel(postProcessor: PostProcessor, maxThreads: Int, queueSize: Int, genBCode: GenBCode, profiler: Profiler): GeneratedClassHandler = {
    val additionalThreads = maxThreads - 1
    val javaExecutor = ProfiledThreadPool.newExecutor(genBCode, profiler, additionalThreads, queueSize, "gen-class-handler")
    new AsyncWritingClassHandler(postProcessor, javaExecutor)
  }

  def withGlobalOptimizations(handler: GeneratedClassHandler): GeneratedClassHandler =
    new GlobalOptimisingGeneratedClassHandler(handler)

  private class GlobalOptimisingGeneratedClassHandler(underlying: GeneratedClassHandler) extends GeneratedClassHandler {
    override val postProcessor: PostProcessor = underlying.postProcessor

    private val generatedUnits = ListBuffer.empty[GeneratedCompilationUnit]

    def process(unit: GeneratedCompilationUnit): Unit = generatedUnits += unit

    def complete(): List[(Throwable, AbstractFile)] = {
      val allGeneratedUnits = generatedUnits.result()
      generatedUnits.clear()
      postProcessor.runGlobalOptimizations(allGeneratedUnits)
      allGeneratedUnits.foreach(underlying.process)
      underlying.complete()
    }

    override def close(): Unit = underlying.close()

    override def toString: String = s"GloballyOptimising[$underlying]"
  }

  private sealed abstract class WritingClassHandler(val javaExecutor: Executor) extends GeneratedClassHandler {
    def tryStealing: Option[Runnable]

    private val processingUnits = ListBuffer.empty[CompilationUnitInPostProcess]

    def process(unit: GeneratedCompilationUnit): Unit = {
      given ExecutionContext = executionContext

      // We want to release these for GC as soon as their processing is done, even if the Future is still referenced
      val classesRef = scala.runtime.ObjectRef(unit.classes)
      val tastyRef = scala.runtime.ObjectRef(unit.tasty)

      val task = Future:
        classesRef.elem.foreach(postProcessor.sendToDisk)
        classesRef.elem = null
        tastyRef.elem.foreach(postProcessor.sendToDisk)
        tastyRef.elem = null

      processingUnits += new CompilationUnitInPostProcess(unit.sourceFile, task)
    }

    private val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(javaExecutor)

    private def takeProcessingUnits(): List[CompilationUnitInPostProcess] = {
      val result = processingUnits.result()
      processingUnits.clear()
      result
    }

    final def complete(): List[(Exception, AbstractFile)] = {
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
      takeProcessingUnits().flatMap { unitInPostProcess =>
        try
          stealWhileWaiting(unitInPostProcess)
          // We know the future is complete, throw the exception if it completed with a failure
          unitInPostProcess.task.value.get.get
          Nil
        catch
          case _: ClosedByInterruptException => throw new InterruptedException()
          case e: Exception => List((e, unitInPostProcess.sourceFile))
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

  /**
   * State for a compilation unit being post-processed.
   * Keeps a reference to the future that runs the post-processor.
   */
  final private class CompilationUnitInPostProcess(val sourceFile: AbstractFile, val task: Future[Unit])
}
