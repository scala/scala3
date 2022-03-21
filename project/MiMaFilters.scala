
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Experimental APIs that can be added in 3.2.0
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
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#CompilationInfoModule.XmacroSettings"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#CompilationInfoModule.XmacroSettings"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.deriving.Mirror.fromProductTyped"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.deriving.Mirror.fromTuple"),

    // Private to the compiler - needed for forward binary compatibility
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.since"),

    // Private inner classes, but we emit all classes as public in Java bytecode
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.quoted.FromExpr$PrimitiveFromExpr"),
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.quoted.Type$ValueOf$"),
    ProblemFilters.exclude[InaccessibleClassProblem]("scala.reflect.Selectable$DefaultSelectable"),
  )
}
