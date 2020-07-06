import scala.quoted._

object SourceFiles {

  type Macro[X] = QuoteContext ?=> Expr[X]
  def tastyContext(using qctx: QuoteContext): QuoteContext = qctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    import qctx.tasty._
    Expr(rootContext.source.getFileName.toString)
  }

}
