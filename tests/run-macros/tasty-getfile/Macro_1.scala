import scala.quoted._


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using s: Scope): s.Expr[String] =
    Expr(s.tasty.Source.path.getFileName.toString)

}
