
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // APIs that must be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),

    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.since"),

    // APIs will be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),
  )
}
