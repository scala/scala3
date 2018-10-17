import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object SourceFiles {

  type Macro[X] = given StagingContext => Expr[X]
  def tastyContext(implicit ctx: StagingContext): StagingContext = ctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val staging = implicitly[StagingContext]
    import staging.reflection._
    rootContext.source.getFileName.toString
  }

}
