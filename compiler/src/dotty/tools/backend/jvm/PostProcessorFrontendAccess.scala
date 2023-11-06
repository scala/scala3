package dotty.tools.backend.jvm

import scala.collection.mutable.{Clearable, HashSet}
import dotty.tools.dotc.util.*
import dotty.tools.dotc.reporting.Message
import dotty.tools.io.AbstractFile
import java.util.{Collection => JCollection, Map => JMap}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.core.Phases

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
  inline final def frontendSynch[T](inline x: T): T = frontendLock.synchronized(x)
}

object PostProcessorFrontendAccess {
  sealed trait CompilerSettings {
    def debug: Boolean
    def target: String // javaOutputVersion

    def dumpClassesDirectory: Option[String]
    def outputDirectory: AbstractFile

    def mainClass: Option[String]

    def jarCompressionLevel: Int
    def backendParallelism: Int
    def backendMaxWorkerQueue: Option[Int]
  }

  sealed trait BackendReporting {
    def error(message: Context ?=> Message, position: SourcePosition): Unit
    def warning(message: Context ?=> Message, position: SourcePosition): Unit
    def log(message: String): Unit

    def error(message: Context ?=> Message): Unit = error(message, NoSourcePosition)
    def warning(message: Context ?=> Message): Unit = warning(message, NoSourcePosition)
  }

  final class BufferingBackendReporting(using Context) extends BackendReporting {
    // We optimise access to the buffered reports for the common case - that there are no warning/errors to report
    // We could use a listBuffer etc - but that would be extra allocation in the common case
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


  class Impl[I <: DottyBackendInterface](val int: I, entryPoints: HashSet[String])(using ctx: Context) extends PostProcessorFrontendAccess {
    lazy val compilerSettings: CompilerSettings = buildCompilerSettings()

    private def buildCompilerSettings(): CompilerSettings = new CompilerSettings {
      extension [T](s: dotty.tools.dotc.config.Settings.Setting[T])
         def valueSetByUser: Option[T] =
           Option(s.value).filter(_ != s.default)
      def s = ctx.settings

      lazy val target =
        val releaseValue = Option(s.javaOutputVersion.value).filter(_.nonEmpty)
        val targetValue = Option(s.XuncheckedJavaOutputVersion.value).filter(_.nonEmpty)
        (releaseValue, targetValue) match
          case (Some(release), None) => release
          case (None, Some(target)) => target
          case (Some(release), Some(_)) =>
            report.warning(s"The value of ${s.XuncheckedJavaOutputVersion.name} was overridden by ${ctx.settings.javaOutputVersion.name}")
            release
          case (None, None) => "8" // least supported version by default

      lazy val debug: Boolean = ctx.debug
      lazy val dumpClassesDirectory: Option[String] = s.Ydumpclasses.valueSetByUser
      lazy val outputDirectory: AbstractFile = s.outputDir.value
      lazy val mainClass: Option[String] = s.XmainClass.valueSetByUser
      lazy val jarCompressionLevel: Int = s.YjarCompressionLevel.value
      lazy val backendParallelism: Int = s.YbackendParallelism.value
      lazy val backendMaxWorkerQueue: Option[Int] = s.YbackendWorkerQueue.valueSetByUser
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
       else local.nn
     }

    object directBackendReporting extends BackendReporting {
      def error(message: Context ?=> Message, position: SourcePosition): Unit = frontendSynch(report.error(message, position))
      def warning(message: Context ?=> Message, position: SourcePosition): Unit = frontendSynch(report.warning(message, position))
      def log(message: String): Unit = frontendSynch(report.log(message))
    }

    def getEntryPoints: List[String] = frontendSynch(entryPoints.toList)
  }
}
