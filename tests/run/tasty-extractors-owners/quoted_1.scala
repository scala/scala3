import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe
import scala.tasty.Tasty
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ~impl('(x))(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl[T](x: Expr[T])(implicit u: Universe): Expr[Unit] = {
    import u._
    import u.tasty._
    val printer = new TastyPrinter(tasty)

    val buff = new StringBuilder

    val output = new TreeTraverser(u.tasty) {
      import tasty._
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree @ DefDef(name, _, _, _, _) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.stringOfTree(tree.owner))
            buff.append("\n\n")
          case tree @ ValDef(name, _, _) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.stringOfTree(tree.owner))
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
