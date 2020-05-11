import scala.quoted._

object SourceFiles {

  type Macro[X] = (s: => Scope) ?=> s.Expr[X]

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] =
    Expr(scope.tasty.Source.path.getFileName.toString)

}
