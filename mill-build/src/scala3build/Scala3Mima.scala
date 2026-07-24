package scala3build

import com.github.lolgab.mill.mima.Mima
import com.github.lolgab.mill.mima.CheckDirection
import mill.api.{BuildCtx, Task}

/**
 * Tasks for configuring MiMA in the Scala 3 build
 *
 * This sets the previous version to check compatibility against, the check direction,
 * and the main MiMA issue filters.
 */
trait Scala3Mima extends Mima {
  def mimaVersion = Scala3Mima.buildMimaVersion

  def mimaPreviousVersions = Seq(Versions.mimaPreviousDottyVersion)
  def mimaCheckDirection = Task {
    CompatMode.value match {
      case CompatMode.BinaryCompatible => CheckDirection.Backward
      case CompatMode.SourceAndBinaryCompatible => CheckDirection.Both
    }
  }

  def mimaExcludeAnnotations = super.mimaExcludeAnnotations() ++ Seq(
    "scala.annotation.experimental"
  )

  def mimaBinaryIssueFilters =
    // Keep these here until we can benefit from lolgab/mill-mima#214
    mimaBackwardIssueFilters().values.toSeq.flatten ++
      mimaForwardIssueFilters().values.toSeq.flatten
}

object Scala3Mima {
  // The sbt-mima-plugin version, that we get from the sbt build, extracting it
  // from project/plugins.sbt.
  private lazy val buildMimaVersion = {
    val subPath = os.sub / "project/plugins.sbt"
    val pluginsSbtLines = BuildCtx.withFilesystemCheckerDisabled {
      os.read.lines(BuildCtx.workspaceRoot / subPath)
    }
    val prefix = """addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % """"
    val suffix = """")"""
    pluginsSbtLines
      .collectFirst {
        case line if line.startsWith(prefix) && line.endsWith(suffix) =>
          line.stripPrefix(prefix).stripSuffix(suffix)
      }
      .getOrElse {
        sys.error(s"Could not find sbt-mima-plugin version in $subPath")
      }
  }
}
