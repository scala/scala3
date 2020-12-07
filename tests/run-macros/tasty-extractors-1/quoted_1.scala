import scala.quoted._

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val tree = Term.of(x)
    val treeStr = Value(tree.showExtractors)
    val treeTpeStr = Value(tree.tpe.showExtractors)

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
