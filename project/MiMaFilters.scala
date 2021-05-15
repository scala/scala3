
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // New APIs marked @experimental in 3.0.1
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TermParamClauseMethods.isErased"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TermParamClauseMethods.isErased"),
    exclude[MissingClassProblem]("scala.annotation.internal.ErasedParam"),
    exclude[MissingClassProblem]("scala.annotation.experimental"),
  )
}
