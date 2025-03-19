
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.unchecked.uncheckedCaptures"),

    // Scala.js only: new runtime support class in 3.2.3; not available to users
    ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.runtime.AnonFunctionXXL"),

    //  New experimental features in 3.3.X
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.clauseInterleaving"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$clauseInterleaving$"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.relaxedExtensionImports"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$relaxedExtensionImports$"),
    // end of New experimental features in 3.3.X
  )
  val TastyCore: Seq[ProblemFilter] = Seq(
    // Backported in 3.3.6
    ProblemFilters.exclude[MissingClassProblem]("dotty.tools.tasty.TastyVersion"),
    ProblemFilters.exclude[MissingClassProblem]("dotty.tools.tasty.TastyVersion$"),
  )
  val Interfaces: Seq[ProblemFilter] = Seq(
  )
}
