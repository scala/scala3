
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // Internals added in 3.0.2
    exclude[MissingClassProblem]("scala.annotation.internal.ProvisionalSuperClass"),

    // New APIs marked @experimental in 3.0.2
    exclude[MissingClassProblem]("scala.Selectable$WithoutPreciseParameterTypes")
  )
}
