
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.unsafeBox"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.unsafeUnbox"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.CanEqual.canEqualMap"),
    ProblemFilters.exclude[MissingClassProblem]("scala.caps$Pure"),
    ProblemFilters.exclude[MissingClassProblem]("scala.caps$unsafe$"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.3-migration"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.3"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E3$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E3$minusmigration$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.boundary"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.boundary$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.boundary$Break"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.boundary$Label"),

    // Scala.js only: new runtime support class in 3.2.3; not available to users
    ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.runtime.AnonFunctionXXL"),

    //  New experimental features in 3.3.X
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.clauseInterleaving"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$clauseInterleaving$"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.into"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$into$"),
    // end of New experimental features in 3.3.X
  )
  val TastyCore: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.TastyBuffer.reset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.TastyFormat.APPLYsigpoly"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.TastyHash.pjwHash64"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.util.Util.dble")
  )
  val Interfaces: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem]("dotty.tools.dotc.interfaces.Diagnostic.diagnosticRelatedInformation"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.dotc.interfaces.Diagnostic.diagnosticRelatedInformation"),
    ProblemFilters.exclude[MissingClassProblem]("dotty.tools.dotc.interfaces.DiagnosticRelatedInformation")
  )
}
