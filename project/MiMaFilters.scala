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
        // IArray integration with Scala Collections:
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.generic.IsSeq.iarrayIsSeq"),
    ))

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // In general we should never have backwards incompatible changes in the library.
      // Only exceptional cases should be added here.

      // Breaking changes since last reference version
      Versions.mimaPreviousDottyVersion -> Seq(

        // scala/scala3#26100
        ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.LazyList"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.LazyList$MidEvaluation$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.LazyList.<clinit>"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.LazyListIterable"),
        ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.LazyListIterable$MidEvaluation$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.LazyListIterable.<clinit>"),

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
