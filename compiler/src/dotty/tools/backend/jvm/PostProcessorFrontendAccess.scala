package dotty.tools
package backend.jvm

import scala.collection.mutable.{Clearable, HashSet}
import dotty.tools.dotc.util.*
import dotty.tools.dotc.reporting.Message
import dotty.tools.io.AbstractFile

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.classpath.*
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Phases
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

  def findClassFile(name: String): Option[io.AbstractFile]

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

    def optUseAnalyzerCache: Boolean

    def optUnreachableCode: Boolean
    def optNullnessTracking: Boolean
    def optBoxUnbox: Boolean
    def optCopyPropagation: Boolean
    def optRedundantCasts: Boolean
    def optSimplifyJumps: Boolean
    def optCompactLocals: Boolean
    def optClosureInvocations: Boolean
    def optAllowSkipCoreModuleInit: Boolean
    def optAssumeModulesNonNull: Boolean
    def optAllowSkipClassLoading: Boolean

    def optInlinerEnabled: Boolean
    def optInlineFrom: List[String]
    def optInlineHeuristics: String

    def optWarningNoInlineMixed: Boolean
    def optWarningNoInlineMissingBytecode: Boolean
    def optWarningNoInlineMissingScalaInlineInfoAttr: Boolean
    def optWarningEmitAtInlineFailed: Boolean
    def optWarningEmitAnyInlineFailed: Boolean

    def optLogInline: Option[String]
    def optTrace: Option[String]

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

      override def optUseAnalyzerCache: Boolean =  s.opt.valueSetByUser.nonEmpty && (optInlinerEnabled || optClosureInvocations || s.opt.value.size > 1)
      override def optUnreachableCode: Boolean = s.optUnreachableCode
      override def optNullnessTracking: Boolean = s.optNullnessTracking
      override def optBoxUnbox: Boolean = s.optBoxUnbox
      override def optCopyPropagation: Boolean = s.optCopyPropagation
      override def optRedundantCasts: Boolean = s.optRedundantCasts
      override def optSimplifyJumps: Boolean = s.optSimplifyJumps
      override def optCompactLocals: Boolean = s.optCompactLocals
      override def optClosureInvocations: Boolean = s.optClosureInvocations
      override def optAllowSkipCoreModuleInit: Boolean = s.optAllowSkipCoreModuleInit
      override def optAssumeModulesNonNull: Boolean = s.optAssumeModulesNonNull
      override def optAllowSkipClassLoading: Boolean = s.optAllowSkipClassLoading
      override def optInlinerEnabled: Boolean = s.optInline.value.nonEmpty
      override def optInlineFrom: List[String] = s.optInline.value
      override def optInlineHeuristics: String = s.optInlineHeuristics.value
      override def optWarningNoInlineMixed: Boolean = s.optWarningNoInlineMixed
      override def optWarningNoInlineMissingBytecode: Boolean = s.optWarningNoInlineMissingBytecode
      override def optWarningNoInlineMissingScalaInlineInfoAttr: Boolean = s.optWarningNoInlineMissingScalaInlineInfoAttr
      override def optWarningEmitAtInlineFailed: Boolean = s.optWarningEmitAtInlineFailed
      override def optWarningEmitAnyInlineFailed: Boolean = s.optWarningEmitAnyInlineFailed
      override def optLogInline: Option[String] = s.YoptLogInline.valueSetByUser
      override def optTrace: Option[String] = s.YoptTrace.valueSetByUser
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

    override def directBackendReporting = DirectBackendReporting(this)(using ctx)

    override def getEntryPoints: List[String] = frontendSynch(entryPoints.toList)(using ctx)

    /* Create a class path for the backend, based on the given class path.
     * Used to make classes available to the inliner's bytecode repository.
     * In particular, if ct.sym is used for compilation, replace it with jrt.
     */
    private lazy val optimizerClassPath = ctx.platform.classPath(using ctx) match {
      case cp @ AggregateClassPath(entries) if entries.head.isInstanceOf[CtSymClassPath] =>
        JrtClassPath(release = None) match {
          case Some(jrt) => AggregateClassPath(entries.drop(1).prepended(jrt))
          case _ => cp
        }
      case cp => cp
    }

    override def findClassFile(name: String): Option[io.AbstractFile] =
      optimizerClassPath.findClassFile(name)
  }
}
