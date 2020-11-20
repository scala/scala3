import scala.quoted._


object Macros {

  implicit inline def printTypes[T](inline x: T): Unit =
    ${impl('x)}

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val buff = new StringBuilder
    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match {
        case tree: TypeBoundsTree =>
          buff.append(tree.tpe.showExtractors)
          buff.append("\n\n")
          traverseTreeChildren(tree)(owner)
        case tree: TypeTree =>
          buff.append(tree.tpe.showExtractors)
          buff.append("\n\n")
          traverseTreeChildren(tree)(owner)
        case _ =>
          super.traverseTree(tree)(owner)
      }
    }

    val tree = Term.of(x)
    traverser.traverseTree(tree)(Symbol.spliceOwner)
    '{print(${Expr(buff.result())})}
  }
}
