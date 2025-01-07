
import com.typesafe.tools.mima.core._

object MiMaFilters {

  object Scala3Library {

    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Build.mimaPreviousDottyVersion -> Seq(
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.betterFors"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$betterFors$"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.quotedPatternsWithPolymorphicFunctions"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$quotedPatternsWithPolymorphicFunctions$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.Patterns.higherOrderHoleWithTypes"),
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.preview"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.preview"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$preview$"),
      ),

      // Additions since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq(
        ProblemFilters.exclude[MissingClassProblem]("scala.NamedTuple"),
        ProblemFilters.exclude[MissingClassProblem]("scala.NamedTuple$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.NamedTupleDecomposition"),
        ProblemFilters.exclude[MissingClassProblem]("scala.NamedTupleDecomposition$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.ValOrDefDefMethods"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.ValOrDefDefTypeTest"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#defnModule.FunctionClass"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#defnModule.PolyFunctionClass"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#FlagsModule.AbsOverride"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.paramVariance"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeLambdaMethods.paramVariances"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.dealiasKeepOpaques"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Tuples.reverse"),
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.internal.AssignedNonLocally"),
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.internal.CaptureChecked"),
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.internal.reachCapability"),
        ProblemFilters.exclude[MissingClassProblem]("scala.annotation.unchecked.uncheckedCaptures"),
        ProblemFilters.exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$ValOrDefDefMethods"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E4$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E4$minusmigration$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E5$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E5$minusmigration$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$clauseInterleaving$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$relaxedExtensionImports$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.scalajs.runtime.AnonFunctionXXL"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.4-migration"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.4"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.5-migration"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.5"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.clauseInterleaving"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.relaxedExtensionImports"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E6$minusmigration$"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.6-migration"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E6$"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.6"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.annotation.experimental.this"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.compiletime.package#package.deferred"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.MethodTypeKind"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleTypeTypeTest"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleType"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleTypeMethods"),
        ProblemFilters.exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$FlexibleTypeMethods"),
        ProblemFilters.exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$FlexibleTypeModule"),
        ProblemFilters.exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$MethodTypeKind"),
        ProblemFilters.exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$MethodTypeKind$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeMethods.isContextual"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeMethods.methodTypeKind"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeModule.apply"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.isSuperAccessor"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.namedTuples"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.modularity"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.betterMatchTypeExtractors"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$betterMatchTypeExtractors$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$modularity$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$namedTuples$"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.7-migration"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.7"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E7$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E7$minusmigration$"),
      ),
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // In general we should never have backwards incompatible changes in the library.
      // Only exceptional cases should be added here.

      // Breaking changes since last reference version
      Build.mimaPreviousDottyVersion -> // Seq.empty, // We should never break backwards compatibility
        Seq(
          // `ReversedMissingMethodProblem`s are acceptable. See comment in `Breaking changes since last LTS`.
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleTypeTypeTest"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.FlexibleTypeMethods"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.isSuperAccessor"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.MethodTypeKind"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeModule.apply"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeMethods.methodTypeKind"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#MethodTypeMethods.isContextual"),
          // Change `experimental` annotation to a final class
          ProblemFilters.exclude[FinalClassProblem]("scala.annotation.experimental"),
        ),

      // Breaking changes since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq(
        // Quotes is assumed to only be implemented by the compiler and on the same version of the library.
        // It is exceptionally OK to break this compatibility. In these cases, there add new abstract methods that would
        // potentially not be implemented by others. If some other library decides to implement these,
        // they need to recompile and republish on each minor release.
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.ValOrDefDefMethods"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.ValOrDefDefTypeTest"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#defnModule.FunctionClass"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#defnModule.PolyFunctionClass"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#FlagsModule.AbsOverride"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.paramVariance"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeLambdaMethods.paramVariances"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeReprMethods.dealiasKeepOpaques"),
      ),
    )
  }

  object TastyCore {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of tasty core
      Build.mimaPreviousDottyVersion -> Seq(
        ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.TastyFormat.FLEXIBLEtype"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("dotty.tools.tasty.TastyFormat.TRACKED"),
      ),

      // Additions since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq(
      )
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Breaking changes since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq.empty // We should never break backwards compatibility
    )
  }

  object Interfaces {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of interfaces
      Build.mimaPreviousDottyVersion -> Seq(
      ),

      // Additions since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq(
      )
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Breaking changes since last LTS
      Build.mimaPreviousLTSDottyVersion -> Seq.empty // We should never break backwards compatibility
    )
  }

}
