import scala.quoted._
import scala.tasty.Tasty

object SourceFiles {

  type Macro[X] = implicit Tasty => Expr[X]
  def tastyContext(implicit ctx: Tasty): Tasty = ctx

  implicit inline def getThisFile: String =
    ~getThisFileImpl

  def getThisFileImpl: Macro[String] = {
    val tasty = tastyContext
    import tasty._
    rootContext.source.getFileName.toString.toExpr
  }


}
