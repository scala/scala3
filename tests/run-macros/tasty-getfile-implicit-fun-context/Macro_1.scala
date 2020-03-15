import scala.quoted._
import scala.quoted.autolift

object SourceFiles {

  type Macro[X] = QuoteContext ?=> Expr[X]
  def tastyContext(using qctx: QuoteContext): QuoteContext = qctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    import qctx.tasty._
    rootContext.source.getFileName.toString
  }

}
