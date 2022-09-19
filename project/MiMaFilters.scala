
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.internal.MappedAlternative"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getOffsetStatic"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticFieldOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getStaticFieldOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.objCAS"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.evaluating"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getStaticOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.nullValue"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.objCas"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waiting"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingAwaitRelease"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingRelease"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$LazyValControlState"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.controlState"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Evaluating$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$NullValue$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Waiting"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.Evaluating"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.NullValue"),

    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.pureFunctions"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.captureChecking"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$pureFunctions$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$captureChecking$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.caps"),
  )
}
