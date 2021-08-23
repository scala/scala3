
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // New APIs that will be introduced in 3.1.0
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.Wildcard"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule.WildcardTypeTest"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.getJPath"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.name"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.path"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.getJPath"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.name"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SourceFileMethods.path"),
    exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#UnapplyModule.apply"),
    exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#UnapplyModule.apply"),
  )
}
