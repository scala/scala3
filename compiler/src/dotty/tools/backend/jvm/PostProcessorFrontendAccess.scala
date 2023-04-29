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
  import PostProcessorFrontendAccess._

  def compilerSettings: CompilerSettings
  def backendReporting: BackendReporting
  def getEntryPoints: List[String]

  private val frontendLock: AnyRef = new Object()
  inline final def frontendSynch[T](inline x: => T): T = frontendLock.synchronized(x)
}

object PostProcessorFrontendAccess {
  sealed trait CompilerSettings {
    def debug: Boolean
    def target: String // javaOutputVersion

    def dumpClassesDirectory: Option[String]
    def outputDirectory: AbstractFile

    def mainClass: Option[String]
  }

  sealed trait BackendReporting {
    def error(message: Context ?=> Message): Unit
    def warning(message: Context ?=> Message): Unit
    def log(message: Context ?=> String): Unit
  }

  class Impl[I <: DottyBackendInterface](val int: I, entryPoints: HashSet[String]) extends PostProcessorFrontendAccess {
    import int.given
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
     }

    object backendReporting extends BackendReporting {
      def error(message: Context ?=> Message): Unit = frontendSynch(report.error(message))
      def warning(message: Context ?=> Message): Unit = frontendSynch(report.warning(message))
      def log(message: Context ?=> String): Unit = frontendSynch(report.log(message))
    }

    def getEntryPoints: List[String] = frontendSynch(entryPoints.toList)
  }
}