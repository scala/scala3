import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object SourceFiles {

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  private def getThisFileImpl(implicit staging: StagingContext): Expr[String] = {
    import staging.reflection._
    rootContext.source.getFileName.toString
  }

}
