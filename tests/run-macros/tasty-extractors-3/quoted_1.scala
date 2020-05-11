import scala.quoted._


object Macros {

  implicit inline def printTypes[T](inline x: T): Unit =
    ${impl('x)}

  def impl[T](using s: Scope)(x: s.Expr[T]): s.Expr[Unit] = {
    import s.tasty._

    val buff = new StringBuilder
    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case tree: TypeBoundsTree =>
          buff.append(tree.tpe.showExtractors)
          buff.append("\n\n")
          traverseTreeChildren(tree)
        case tree: TypeTree =>
          buff.append(tree.tpe.showExtractors)
          buff.append("\n\n")
          traverseTreeChildren(tree)
        case _ =>
          super.traverseTree(tree)
      }
    }

    val tree = x
    traverser.traverseTree(tree)
    '{print(${Expr(buff.result())})}
  }
}
