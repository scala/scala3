import scala.quoted._

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val tree = x.asTerm

    val treeStr = Expr(tree.showExtractors)
    val treeTpeStr = Expr(tree.tpe.showExtractors)

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
