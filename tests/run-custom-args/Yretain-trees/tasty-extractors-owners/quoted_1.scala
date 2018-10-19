import scala.quoted._

import scala.tasty._
import scala.tasty.util.TreeTraverser

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val buff = new StringBuilder

    implicit class SymOps(sym: Symbol) {
      def treeOpt: Option[Tree] = sym match {
        case IsClassSymbol(sym) => Some(sym.tree)
        case IsDefSymbol(sym) => Some(sym.tree)
        case IsValSymbol(sym) => Some(sym.tree)
        case _ => None
      }
    }

    val output = new TreeTraverser(tasty) {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.treeOpt.get.show)
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.treeOpt.get.show)
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.toTasty
    output.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }

}
