
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.unsafeBox"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.unsafeUnbox"),
  )
  val TastyCore: Seq[ProblemFilter] = Seq()
  val Interfaces: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[MissingClassProblem]("dotty.tools.dotc.interfaces.DiagnosticRelatedInformation"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("dotty.tools.dotc.interfaces.Diagnostic.diagnosticRelatedInformation")
  )
}
