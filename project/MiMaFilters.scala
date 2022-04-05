
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Experimental APIs that can be added in 3.2.0 or later
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.typeArgs"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.deriving.Mirror.fromProductTyped"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.deriving.Mirror.fromTuple"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#ClassDefModule.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#ClassDefModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolModule.newClass"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolModule.newClass"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),

    // TupledFunction
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.TupledFunctions"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.TupledFunctions$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.TupledFunction"),
    ProblemFilters.exclude[MissingClassProblem]("scala.util.TupledFunction$"),

    // Private inner classes, but we emit all classes as public in Java bytecode
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.quoted.FromExpr$PrimitiveFromExpr"),
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.quoted.Type$ValueOf$"),
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.reflect.Selectable$DefaultSelectable"),
  )
}
