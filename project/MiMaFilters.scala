
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Internals added in 3.0.2
    exclude[MissingClassProblem]("scala.annotation.internal.ProvisionalSuperClass"),

    // New APIs marked @experimental in 3.0.2
    exclude[MissingClassProblem]("scala.Selectable$WithoutPreciseParameterTypes"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.WildcardTypeTest"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.Wildcard"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTreeTypeTest"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTree"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTreeMethods"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.WildcardTypeTest"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.Wildcard"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTreeTypeTest"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTree"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule.TypedTreeMethods"),
    exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$TypedTreeMethods"),
    exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$TypedTreeModule"),
    exclude[MissingClassProblem]("scala.quoted.Quotes$reflectModule$WildcardModule"),
  )
}
