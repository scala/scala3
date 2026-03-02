
import com.typesafe.tools.mima.core._

object MiMaFilters {

  object Scala3Library {

    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Build.mimaPreviousDottyVersion -> Seq(
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.package#package.freeze"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.caps.package#package.cap"),
        ProblemFilters.exclude[FinalClassProblem]("scala.Function1$UnliftOps$"),
        ProblemFilters.exclude[FinalClassProblem]("scala.jdk.Accumulator$AccumulatorFactoryShape$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.ArrayOps.iterateUntilEmpty$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.ArrayOps.scala$collection$ArrayOps$$elemTag$extension"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.language#experimental.safe"),
        ProblemFilters.exclude[MissingClassProblem]("scala.language$experimental$safe$")
    ))

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // In general we should never have backwards incompatible changes in the library.
      // Only exceptional cases should be added here.

      // Breaking changes since last reference version
      Build.mimaPreviousDottyVersion -> Seq(
        ProblemFilters.exclude[MissingTypesProblem]("scala.util.control.NonLocalReturns$ReturnThrowable"),
        // THIS IS FINE, IT SHOULD HAVE BEEN THIS WAY
        ProblemFilters.exclude[MissingTypesProblem]("scala.Function1$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.Function1$UnliftOps$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.Product1$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.Product2$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.collection.ArrayOps$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.jdk.Accumulator$"),
        ProblemFilters.exclude[MissingTypesProblem]("scala.jdk.Accumulator$AccumulatorFactoryShape$"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Function1#UnliftOps.equals$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Function1#UnliftOps.hashCode$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Function1#UnliftOps.unlift$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.ArrayOps.iterateUntilEmpty$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.ArrayOps.scala$collection$ArrayOps$$$iterateUntilEmpty$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.ArrayOps.scala$collection$ArrayOps$$$elemTag$extension"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.anyAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.doubleAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.intAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.jDoubleAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.jIntegerAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.jLongAccumulatorFactoryShape"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("scala.jdk.Accumulator#AccumulatorFactoryShape.longAccumulatorFactoryShape"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.Function1.UnliftOps"),
        ProblemFilters.exclude[MissingFieldProblem]("scala.jdk.Accumulator.AccumulatorFactoryShape"),
        ProblemFilters.exclude[FinalMethodProblem]("scala.jdk.Accumulator.++:"),
        ProblemFilters.exclude[FinalMethodProblem]("scala.jdk.Accumulator.:++"),
        ProblemFilters.exclude[FinalMethodProblem]("scala.jdk.Accumulator.concat"),
        ProblemFilters.exclude[FinalMethodProblem]("scala.jdk.Accumulator.sizeHint$default$2"),
      )
    )
  }

  object ScalaLibrarySJS {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of the library
      Build.mimaPreviousDottyVersion -> Seq(
        // No .class files generated in the artifacts, only `scala.scalajs.*` files might be present
        ProblemFilters.exclude[MissingClassProblem]("scala.*"),
      ),
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // We should never break backwards compatibility
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object TastyCore {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of tasty core
      Build.mimaPreviousDottyVersion -> Seq(
      )
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )
  }

  object Interfaces {
    val ForwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map(
      // Additions that require a new minor version of interfaces
      Build.mimaPreviousDottyVersion -> Seq.empty,
    )

    val BackwardsBreakingChanges: Map[String, Seq[ProblemFilter]] = Map.empty
  }

}
