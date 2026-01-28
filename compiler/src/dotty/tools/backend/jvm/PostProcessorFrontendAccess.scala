package dotty.tools
package backend.jvm

import scala.collection.mutable.{Clearable, HashSet}
import dotty.tools.dotc.util.*
import dotty.tools.dotc.reporting.Message
import dotty.tools.io.AbstractFile

import java.util.{Collection as JCollection, Map as JMap}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.config.ScalaSettings
import BTypes.InternalName

import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * Functionality needed in the post-processor whose implementation depends on the compiler
 * frontend. All methods are synchronized.
 */
sealed abstract class PostProcessorFrontendAccess {
  import PostProcessorFrontendAccess.*

  def compilerSettings: CompilerSettings

  def withThreadLocalReporter[T](reporter: BackendReporting)(fn: => T): T

  def backendReporting: BackendReporting

  def directBackendReporting: BackendReporting

  def getEntryPoints: List[String]

  private val frontendLock: AnyRef = new Object()

  inline final def frontendSynch[T](inline x: Context ?=> T)(using Context): T = frontendLock.synchronized(x)

  inline final def frontendSynchWithoutContext[T](inline x: T): T = frontendLock.synchronized(x)

  inline def perRunLazy[T](inline init: Context ?=> T)(using Context): Lazy[T] = new SynchronizedLazy(this, init)

  def javaDefinedClasses: Set[InternalName]

  def recordPerRunCache[T <: mutable.Clearable](cache: T): T

  def recordPerRunJavaMapCache[T <: JMap[?, ?]](cache: T): T

  def recordPerRunJavaCache[T <: JCollection[?]](cache: T): T
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

    def optAddToBytecodeRepository: Boolean
    def optBuildCallGraph: Boolean
    def optUseAnalyzerCache: Boolean

    def optNone: Boolean

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

  sealed trait BackendReporting {
    def error(message: Context ?=> Message, position: SourcePosition): Unit
    def warning(message: Context ?=> Message, position: SourcePosition): Unit
    def log(message: String): Unit

    def error(message: Context ?=> Message): Unit = error(message, NoSourcePosition)
    def warning(message: Context ?=> Message): Unit = warning(message, NoSourcePosition)
  }

  final class BufferingBackendReporting(using Context) extends BackendReporting {
    // We optimize access to the buffered reports for the common case - that there are no warning/errors to report
    // We could use a listBuffer etc. - but that would be extra allocation in the common case
    // Note - all access is externally synchronized, as this allow the reports to be generated in on thread and
    // consumed in another
    private var bufferedReports = List.empty[Report]
    enum Report(val relay: BackendReporting => Unit):
      case Error(message: Message, position: SourcePosition) extends Report(_.error(message, position))
      case Warning(message: Message, position: SourcePosition) extends Report(_.warning(message, position))
      case Log(message: String) extends Report(_.log(message))

    def error(message: Context ?=> Message, position: SourcePosition): Unit = synchronized:
      bufferedReports ::= Report.Error(message, position)

    def warning(message: Context ?=> Message, position: SourcePosition): Unit = synchronized:
      bufferedReports ::= Report.Warning(message, position)

    def log(message: String): Unit = synchronized:
      bufferedReports ::= Report.Log(message)

    def relayReports(toReporting: BackendReporting): Unit = synchronized:
      if bufferedReports.nonEmpty then
        bufferedReports.reverse.foreach(_.relay(toReporting))
        bufferedReports = Nil
  }


  class Impl(entryPoints: mutable.HashSet[String])(using ctx: Context) extends PostProcessorFrontendAccess {
    override def compilerSettings: CompilerSettings = _compilerSettings.get
    private lazy val _compilerSettings: Lazy[CompilerSettings] = perRunLazy(buildCompilerSettings)

    def recordPerRunCache[T <: mutable.Clearable](cache: T): T = cache // TODO FIX ME: This doesn't cache anything

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

    override object directBackendReporting extends BackendReporting {
      def error(message: Context ?=> Message, position: SourcePosition): Unit = frontendSynch(report.error(message, position))
      def warning(message: Context ?=> Message, position: SourcePosition): Unit = frontendSynch(report.warning(message, position))
      def log(message: String): Unit = frontendSynch(report.log(message))
    }

    def getEntryPoints: List[String] = frontendSynch(entryPoints.toList)

    def javaDefinedClasses: Set[InternalName] = frontendSynch {
      currentRun.symSource.keys.iterator.collect {
        case sym if sym.isJavaDefined => sym.javaBinaryNameString
      }.toSet
    }
  }
}
