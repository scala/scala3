
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Experimental APIs that can be added in 3.2.0 or later
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.append"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#ClassDefModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolModule.newClass"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),

    // Private to the compiler - needed for forward binary compatibility
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.since"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getStaticOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.objCAS"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.objCAS"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.evaluating"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.getStaticOffset"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.nullValued"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.objCas"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waiting"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingAwaitRelease"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals#Names.waitingRelease"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Evaluating$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$NULL$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.LazyVals$Waiting"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.Evaluating"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.NULL")
  )
}
