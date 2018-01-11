package dotty.tools.dotc.profile

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts.Context

object InPhase {
  val idGen = new AtomicInteger
}
/**
 * A wrapper to allow actions to be associated to a Phase. This aids profiling, particularly where a actions occur in
 * multiple threads, or out of order
 *
 * When you are running a compilation task that involved some activity on a background thread
 * (not the one running [[compileUnits]]) the profiler is not aware of that thread and so cannot account
 * for the activity.
 *
 * By wrapping the activity in this class or one of it children the profiler (if enabled) is informed
 * and the statistics can be gathered
 *
 * No InPhase should run concurrently with another InPhase on the same thread - the statistics dont cope with nesting
 */
sealed abstract class InPhase(val phase: Phase, val comment: String)(implicit ctx: Context) {

  private[profile] final val id = InPhase.idGen.incrementAndGet()
  private[profile] final val profiler = ??? // get profiler
  private[profile] final var idleNs = 0L
//  profiler.registerInPhase(this)

//  @inline protected [profile] def doAction[T] (fn : => T) : T = {
//    val before = profiler.beforeInPhase(this)
//    try fn
//    finally profiler.afterInPhase(this, before, idleNs)
//  }

  /**
   * If the compilation activity has some idle time waiting on a future, then this can be recorded by
   * using this method to perform the wait for you. This allow the profiler to distinguish idle time (waiting for some
   * related activity to complete), from for example waiting on I/O
   * @param future the future that you are waiting on
   * @param duration the maximum duration to wait
   */
  def idle(future: Future[_], duration:Duration = Duration.Inf): Unit = {
    if (!future.isCompleted) {
      val start = System.nanoTime()
      try Await.ready(future, duration)
      finally idleNs += (System.nanoTime() - start)
    }
  }

}
/**
 * an InPhase for Runnables
 *
 * By enclosing the activity in the doRun method of this class the profiler (if enabled) is informed
 * and the statistics can be gathered
 */

//object RunnableInPhase {
//  def apply(phase: Phase, comment: String)(fn: => Unit)(implicit executionContext: ExecutionContext, ctx: Context) = {
//    new RunnableInPhase(phase, comment)(fn)
//  }
//}
//class RunnableInPhase(phase:Phase, comment:String)(fn: => Unit)(implicit ctx: Context) extends InPhase(phase, comment) with Runnable {
//  final def run(): Unit = doAction(fn)
//}

/**
 * an InPhase for Futures
 *
 * By enclosing the activity in this wrapper the profiler (if enabled) is informed
 * and the statistics can be gathered
 */
//object FutureInPhase {
//  def apply[T](phase:Phase, comment:String)(fn: => T)(implicit executionContext: ExecutionContext, ctx: Context) = {
//    val inPhase = new FutureInPhase(phase, comment)(fn)
//    Future(inPhase.exec())
//  }
//}

//class FutureInPhase[T](phase:Phase, comment:String)(fn: => T)(implicit ctx: Context) extends InPhase(phase, comment) {
//  final def exec() = doAction(fn)
//}