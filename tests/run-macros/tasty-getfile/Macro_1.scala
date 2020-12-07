import scala.quoted._


object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(using Quotes) : Expr[String] =
    Value(quotes.reflect.SourceFile.current.jpath.getFileName.toString)

}
