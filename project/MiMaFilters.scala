// This file is used in the Mill build too. Do not modify this comment.

package dotty.tools.sbtplugin

import com.typesafe.tools.mima.core._

object MiMaFilters {

  object Scala3Library {

    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Versions.mimaPreviousDottyVersion -> Seq(
        // new feature: CanEqual support for NamedTuple
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.NamedTuple.namedTupleCanEqual"),
        // IArray integration with Scala Collections:
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.BuildFrom.buildFromIArray"),
    ))

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // In general we should never have backwards incompatible changes in the library.
      // Only exceptional cases should be added here.

      // Breaking changes since last reference version
      Versions.mimaPreviousDottyVersion -> Seq(
      )
    )
  }

  object ScalaLibrarySJS {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Versions.mimaPreviousDottyVersion -> Seq(
        // No .class files generated in the artifacts, only `scala.scalajs.*` files might be present
        ProblemFilters.exclude[MissingClassProblem]("scala.*"),
      ),
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // We should never break backwards compatibility
      Versions.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object TastyCore {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of tasty core
      Versions.mimaPreviousDottyVersion -> Seq(
      )
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      Versions.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object Interfaces {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of interfaces
      Versions.mimaPreviousDottyVersion -> Seq.empty,
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map.empty
  }

}
