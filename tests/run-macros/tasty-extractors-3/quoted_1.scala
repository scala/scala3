import scala.quoted.*


object Macros {

  implicit inline def printTypes[T](inline x: T): Unit =
    ${impl('x)}

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val buff = new StringBuilder
    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case tree: TypeBoundsTree =>
          buff.append(tree.tpe.show(using Printer.TypeReprStructure))
          buff.append("\n\n")
          traverseTreeChildren(tree)(owner)
        case tree: TypeTree =>
          buff.append(tree.tpe.show(using Printer.TypeReprStructure))
          buff.append("\n\n")
          traverseTreeChildren(tree)(owner)
        case _ =>
          super.traverseTree(tree)(owner)
      }
    }

    val tree = x.asTerm
    traverser.traverseTree(tree)(Symbol.spliceOwner)
    '{print(${Expr(buff.result())})}
  }
}
