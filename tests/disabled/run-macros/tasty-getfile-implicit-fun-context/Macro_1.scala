import scala.quoted._

object SourceFiles {

  type Macro[X] = (s: Scope) ?=> s.Expr[X]
  def tastyScope(using s: Scope): s.type = s

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val s = tastyScope
    Expr(s.tasty.Source.path.getFileName.toString)
  }

}
