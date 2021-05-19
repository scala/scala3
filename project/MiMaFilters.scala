
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // APIs that must be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),

    // Experimental APIs that can be added in 3.2.0
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.append"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#ClassDefModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolModule.newClass"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),

    // Experimental `MainAnnotation` APIs. Can be added in 3.3.0 or later.
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$Command"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$CommandInfo"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$ParameterInfo"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$ParameterAnnotation"),
  )
}
