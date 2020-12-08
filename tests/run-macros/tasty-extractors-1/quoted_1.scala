import scala.quoted._

object Macros {

  implicit inline def printTree[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val tree = x.asTerm
    val treeStr = Expr(tree.show(using Printer.TreeStructure))
    val treeTpeStr = Expr(tree.tpe.show(using Printer.TypeReprStructure))

    '{
      println(${treeStr})
      println(${treeTpeStr})
      println()
    }
  }
}
