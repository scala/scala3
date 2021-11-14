
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Experimental APIs that can be added in 3.2.0
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.init"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.last"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.append"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.substituteTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.substituteTypes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.typeArgs"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.typeArgs"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.double"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.double$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.float"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.float$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.long"),
    ProblemFilters.exclude[MissingClassProblem]("scala.compiletime.ops.long$"),
    // LazyVals
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
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyVals.NULL"),
  )
}
