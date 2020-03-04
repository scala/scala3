import scala.quoted._
import scala.quoted.autolift.{given _}


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using qctx: QuoteContext) : Expr[String] = {
    import qctx.tasty._
    summon[Context].source.getFileName.toString
  }

}
