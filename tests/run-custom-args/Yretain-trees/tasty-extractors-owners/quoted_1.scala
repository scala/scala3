import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val buff = new StringBuilder

    implicit class SymOps(sym: Symbol) {
      def treeOpt: Option[Tree] = sym match {
        case IsClassDefSymbol(sym) => Some(sym.tree)
        case IsDefDefSymbol(sym) => Some(sym.tree)
        case IsValDefSymbol(sym) => Some(sym.tree)
        case _ => None
      }
    }

    val output = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.treeOpt.get.showExtractors)
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.treeOpt.get.showExtractors)
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
