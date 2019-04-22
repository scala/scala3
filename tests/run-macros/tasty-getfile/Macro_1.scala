import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    rootContext.source.getFileName.toString
  }

}
