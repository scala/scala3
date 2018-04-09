import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.names.Name
import scala.tasty.trees._
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ~impl('(x))(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl[T](x: Expr[T])(implicit ctx: Context): Expr[Unit] = {
    val buff = new StringBuilder
    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree @ DefDef(name: Name, _, _, _, _) =>
            buff.append(TastyPrinter.stringOf(name))
            buff.append("\n")
            buff.append(TastyPrinter.stringOf(tree.owner))
            buff.append("\n\n")
          case tree @ ValDef(name: Name, _, _) =>
            buff.append(TastyPrinter.stringOf(name))
            buff.append("\n")
            buff.append(TastyPrinter.stringOf(tree.owner))
            buff.append("\n\n")
          case _ =>
        }
        traverseChildren(tree)
      }
    }

    val tree = x.toTasty
    traverser.traverse(tree)(ctx)
    '(print(~buff.result().toExpr))
  }
}
