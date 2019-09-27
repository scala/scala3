import scala.quoted._
import scala.quoted.autolift.given

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._

    val buff = new StringBuilder

    val output = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.showExtractors)
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.showExtractors)
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.unseal
    output.traverseTree(tree)
    '{print(${buff.result()})}
  }

}
