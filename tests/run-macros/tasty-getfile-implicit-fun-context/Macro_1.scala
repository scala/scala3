import scala.quoted._
import scala.quoted.autolift.{given _}

object SourceFiles {

  type Macro[X] = QuoteContext ?=> Expr[X]
  def tastyContext with (qctx: QuoteContext) : QuoteContext = qctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    import qctx.tasty.{_, given _}
    rootContext.source.getFileName.toString
  }

}
