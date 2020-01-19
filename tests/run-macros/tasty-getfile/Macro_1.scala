import scala.quoted._
import scala.quoted.autolift.{given _}


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl with (qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty.{_, given _}
    rootContext.source.getFileName.toString
  }

}
