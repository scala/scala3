import scala.quoted.*

object SourceFiles {

  type Macro[X] = Quotes ?=> Expr[X]
  def tastyContext(using q: Quotes): Quotes = q

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val qctx = tastyContext
    Expr(qctx.reflect.SourceFile.current.jpath.getFileName.toString)
  }

}
