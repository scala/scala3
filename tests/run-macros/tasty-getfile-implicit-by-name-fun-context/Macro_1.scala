import scala.quoted._

object SourceFiles {

  type Macro[X] = (=> QuoteContext) ?=> Expr[X]

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] =
    Expr(reflect.Source.path.getFileName.toString)

}
