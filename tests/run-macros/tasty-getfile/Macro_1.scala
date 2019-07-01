import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl given (qctx: QuoteContext): Expr[String] = {
    import qctx.tasty._
    rootContext.source.getFileName.toString
  }

}
