
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[Problem]("scala.runtime.*"), // KEEP: scala.runtime isn't public API

    // APIs that must be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),

    // Experimental `MainAnnotation` APIs. Can be added in 3.3.0 or later.
    // MiMa bug: classes nested in an experimental object should be ignored
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$Info"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$Parameter"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$ParameterAnnotation"),
  )
}
