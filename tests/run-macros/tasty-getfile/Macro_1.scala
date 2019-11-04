import scala.quoted._
import scala.quoted.autolift.given


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{_, given}
    rootContext.source.getFileName.toString
  }

}
