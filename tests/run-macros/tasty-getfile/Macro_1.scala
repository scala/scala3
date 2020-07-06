import scala.quoted._


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    Expr(summon[Context].source.getFileName.toString)
  }

}
