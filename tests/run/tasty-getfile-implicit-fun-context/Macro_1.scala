import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object SourceFiles {

  type Macro[X] = given Reflection => Expr[X]
  def tastyContext(implicit ctx: Reflection): Reflection = ctx

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] = {
    val reflect = tastyContext
    import reflect._
    rootContext.source.getFileName.toString
  }


}
