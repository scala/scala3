package scala3build

import com.github.lolgab.mill.mima.Mima
import com.github.lolgab.mill.mima.CheckDirection
import mill.api.Task

/**
 * Tasks for configuring MiMA in the Scala 3 build
 *
 * This sets the previous version to check compatibility against, the check direction,
 * and the main MiMA issue filters.
 */
trait Scala3Mima extends Mima {
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
