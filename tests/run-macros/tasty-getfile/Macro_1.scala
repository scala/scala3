import scala.quoted._


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using qctx: QuoteContext) : Expr[String] =
    Expr(qctx.tasty.Source.path.getFileName.toString)

}
