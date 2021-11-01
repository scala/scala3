
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.init"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.last"),

    // Should have been added in 3.1.0
    // These are only allowed on imports and therefore should not be present in binaries emitted before
    // this addition of these members and therefore should not cause any conflicts.
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.1-migration"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.1"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E1$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E1$minusmigration$"),
  )
}
