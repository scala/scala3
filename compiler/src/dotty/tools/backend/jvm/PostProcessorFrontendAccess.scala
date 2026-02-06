package dotty.tools
package backend.jvm

import scala.collection.mutable.HashSet
import dotty.tools.io.AbstractFile

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.report
import dotty.tools.dotc.config.ScalaSettings

import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * Functionality needed in the post-processor whose implementation depends on the compiler
 * frontend. All methods are synchronized.
 */
sealed abstract class PostProcessorFrontendAccess(val ctx: FreshContext) {
  import PostProcessorFrontendAccess.*

  def compilerSettings: CompilerSettings

  def withThreadLocalReporter[T](reporter: BackendReporting)(fn: => T): T

  def backendReporting: BackendReporting

  def directBackendReporting: BackendReporting

  def getEntryPoints: List[String]

  private val frontendLock: AnyRef = new Object()

  inline final def frontendSynch[T](inline x: Context ?=> T)(using Context): T = frontendLock.synchronized(x)

  inline final def frontendSynchWithoutContext[T](inline x: T): T = frontendLock.synchronized(x)

  def perRunLazy[T](init: Context ?=> T)(using Context): Lazy[T] = new SynchronizedLazy(this, init)
}

object PostProcessorFrontendAccess {
  abstract class Lazy[T] {
    def get: T
  }

  /** Does not synchronize on the frontend. (But still synchronizes on itself, so terrible name) */
  class LazyWithoutLock[T](init: => T) extends Lazy[T] {
    @volatile private var isInit: Boolean = false
    private var v: T = uninitialized

    override def get: T =
      if isInit then v
      else this.synchronized {
        if !isInit then v = init
        isInit = true
        v
      }
  }

  /** A container for value with lazy initialization synchronized on compiler frontend
   * Used for sharing variables requiring a Context for initialization, between different threads
   * Similar to Scala 2 BTypes.LazyVar, but without re-initialization of BTypes.LazyWithLock. These were not moved to PostProcessorFrontendAccess only due to problematic architectural decisions.
   */
  private class SynchronizedLazy[T](frontendAccess: PostProcessorFrontendAccess, init: Context ?=> T)(using Context) extends Lazy[T] {
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

  sealed trait CompilerSettings {
    def debug: Boolean
    def target: String // javaOutputVersion

    def dumpClassesDirectory: Option[String]
    def outputDirectory: AbstractFile

    def mainClass: Option[String]

    def jarCompressionLevel: Int
    def backendParallelism: Int
    def backendMaxWorkerQueue: Option[Int]
    def outputOnlyTasty: Boolean
  }

  class Impl(entryPoints: mutable.HashSet[String])(ctx: FreshContext) extends PostProcessorFrontendAccess(ctx) {
    override def compilerSettings: CompilerSettings = _compilerSettings.get
    private lazy val _compilerSettings: Lazy[CompilerSettings] = perRunLazy(buildCompilerSettings)(using ctx)

    private def buildCompilerSettings(using ctx: Context): CompilerSettings = new CompilerSettings {
      extension [T](s: dotty.tools.dotc.config.Settings.Setting[T])
         def valueSetByUser: Option[T] = Option(s.value).filter(_ != s.default)

      inline def s: ScalaSettings = ctx.settings

      override val target: String =
        val releaseValue = Option(s.javaOutputVersion.value).filter(_.nonEmpty)
        val targetValue = Option(s.XuncheckedJavaOutputVersion.value).filter(_.nonEmpty)
        (releaseValue, targetValue) match
          case (Some(release), None) => release
          case (None, Some(target)) => target
          case (Some(release), Some(_)) =>
            report.warning(s"The value of ${s.XuncheckedJavaOutputVersion.name} was overridden by ${ctx.settings.javaOutputVersion.name}")
            release
          case (None, None) => "17" // least supported version by default

      override val debug: Boolean = ctx.debug
      override val dumpClassesDirectory: Option[String] = s.Xdumpclasses.valueSetByUser
      override val outputDirectory: AbstractFile = s.outputDir.value
      override val mainClass: Option[String] = s.XmainClass.valueSetByUser
      override val jarCompressionLevel: Int = s.XjarCompressionLevel.value
      override val backendParallelism: Int = s.YbackendParallelism.value
      override val backendMaxWorkerQueue: Option[Int] = s.YbackendWorkerQueue.valueSetByUser

      @annotation.nowarn("cat=deprecation")
      override val outputOnlyTasty: Boolean = s.YoutputOnlyTasty.value
     }

     private lazy val localReporter = new ThreadLocal[BackendReporting]

     override def withThreadLocalReporter[T](reporter: BackendReporting)(fn: => T): T = {
       val old = localReporter.get()
       localReporter.set(reporter)
       try fn
       finally
         if old eq null then localReporter.remove()
         else localReporter.set(old)
     }

     override def backendReporting: BackendReporting = {
       val local = localReporter.get()
       if local eq null then directBackendReporting
       else local
     }

    override def directBackendReporting: BackendReporting = DirectBackendReporting(this)(using ctx)

    override def getEntryPoints: List[String] = frontendSynch(entryPoints.toList)(using ctx)
  }
}
