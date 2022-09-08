
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // APIs will be added in 3.3.0
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.precise"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#PolyTypeModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#PolyTypeModule.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeLambdaModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeLambdaModule.apply"),
  )
}
