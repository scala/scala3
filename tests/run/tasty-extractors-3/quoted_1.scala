import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.trees.{Tree, TypeBoundsTree, TypeTree}
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def printTypes[T](x: => T): Unit =
    ~impl('(x))(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl[T](x: Expr[T])(implicit ctx: Context): Expr[Unit] = {
    val buff = new StringBuilder
    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree: TypeTree =>
            buff.append(TastyPrinter.stringOf(tree.tpe))
            buff.append("\n\n")
          case tree: TypeBoundsTree =>
            buff.append(TastyPrinter.stringOf(tree.tpe))
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
