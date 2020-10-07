import scala.quoted._

object SourceFiles {

  type Macro[X] = QuoteContext ?=> Expr[X]
  def tastyContext(using qctx: QuoteContext): QuoteContext = qctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    Expr(qctx.reflect.Source.path.getFileName.toString)
  }

}
