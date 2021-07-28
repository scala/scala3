import scala.quoted.*

object SourceFiles {

  type Macro[X] = (=> Quotes) ?=> Expr[X]

  implicit inline def getThisFile: String =
    ${getThisFileImpl}

  def getThisFileImpl: Macro[String] =
    val q = quotes // Quotes is ByName and hence not stable (q stabilizes it)
    Expr(q.reflect.SourceFile.current.name)

}
