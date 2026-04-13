package dotty.tools
package backend.jvm

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.util.SrcPos

import scala.compiletime.uninitialized

/**
 * Abstracts the frontend data structures, specially the Context, that need to be accessed in a single-threaded manner.
 */
final class PostProcessorFrontendAccess(val ctx: Context) {
  import PostProcessorFrontendAccess.*

  def optimizerWarning(msg: Context ?=> Message, site: String, pos: SrcPos): Unit =
    report.optimizerWarning(msg(using ctx), site, pos)(using ctx)

  private val frontendLock: AnyRef = new Object()

  private[PostProcessorFrontendAccess] def frontendSynch[T](x: => T): T = frontendLock.synchronized(x)

  def perRunLazy[T](init: => T): Lazy[T] = new SynchronizedLazy(this, init)
}

object PostProcessorFrontendAccess {
  abstract class Lazy[T] {
    def get: T
  }

  /** A container for value with lazy initialization synchronized on compiler frontend
   * Used for sharing variables requiring a Context for initialization, between different threads
   * Similar to Scala 2 BTypes.LazyVar, but without re-initialization of BTypes.LazyWithLock. These were not moved to PostProcessorFrontendAccess only due to problematic architectural decisions.
   */
  private class SynchronizedLazy[T](frontendAccess: PostProcessorFrontendAccess, init: => T) extends Lazy[T] {
    @volatile private var isInit: Boolean = false
    private var v: T = uninitialized

    override def get: T =
      if isInit then v
      else frontendAccess.frontendSynch {
        if !isInit then v = init
        isInit = true
        v
    }
  }
}
