import scala.quoted._

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](using s: Scope)(x: s.Expr[T]): s.Expr[Unit] = {
    import s.tasty._

    val tree = x
    val treeStr = Expr(tree.showExtractors)
    val treeTpeStr = Expr(tree.tpe.showExtractors)

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
