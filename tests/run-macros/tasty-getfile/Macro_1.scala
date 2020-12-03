import scala.quoted._


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using Quotes) : Expr[String] =
    Expr(quotes.reflect.SourceFile.current.jpath.getFileName.toString)

}
