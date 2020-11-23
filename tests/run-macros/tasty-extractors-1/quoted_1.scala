import scala.quoted._

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import qctx.reflect._

    val tree = Term.of(x)
    val treeStr = Expr(tree.showExtractors)
    val treeTpeStr = Expr(tree.tpe.showExtractors)

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
