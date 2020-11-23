import scala.quoted._

object SourceFiles {

  type Macro[X] = (=> Quotes) ?=> Expr[X]

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] =
    Expr(qctx.reflect.Source.path.getFileName.toString)

}
