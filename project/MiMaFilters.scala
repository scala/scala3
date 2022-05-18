
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

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticFieldOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getOffsetStatic"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.objCAS"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.evaluating"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getStaticFieldOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getOffsetStatic"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.nullValued"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.objCas"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waiting"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingAwaitRelease"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingRelease"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Evaluating$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$NULL$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Waiting"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.Evaluating"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.NULL"),

    // Experimental `MainAnnotation` APIs. Can be added in 3.3.0 or later.
    // MiMa bug: classes nested in an experimental object should be ignored
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$Info"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$Parameter"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.MainAnnotation$ParameterAnnotation"),
  )
}
