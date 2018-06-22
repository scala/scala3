import scala.quoted._

import scala.tasty.Universe

object SourceFiles {

  implicit inline def getThisFile: String =
    ~getThisFileImpl(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  private def getThisFileImpl(implicit u: Universe): Expr[String] = {
    import u.tasty._
    u.context.source.getFileName.toString.toExpr
  }

}
