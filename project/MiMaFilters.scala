
import com.typesafe.tools.mima.core._

object MiMaFilters {

  object Scala3Library {

    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Build.mimaPreviousDottyVersion -> Seq(
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.package#package.freeze"),
        // scala/scala3#24545 / scala/scala3#24788
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.unchecked.uncheckedOverride"),
      ),

    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // In general we should never have backwards incompatible changes in the library.
      // Only exceptional cases should be added here.

      // Breaking changes since last reference version
      Build.mimaPreviousDottyVersion -> Seq.empty, // We should never break backwards compatibility
    )
  }

  object ScalaLibrarySJS {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Build.mimaPreviousDottyVersion -> Seq(
        // No .class files generated in the artifacts, only `scala.scalajs.*` files might be present
        ProblemFilters.exclude[MissingClassProblem]("scala.*"),
      ),
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // We should never break backwards compatibility
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object TastyCore {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of tasty core
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object Interfaces {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of interfaces
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map.empty
  }

}
