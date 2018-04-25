
import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted._
import scala.tasty.util.{TastyPrinter, TreeTraverser}
import scala.tasty.trees._
import scala.tasty.Context

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      val x = 1
      val y: x.type = x
      def f1[T](): T = ???
      def f2[T >: Int <: Int](): T = ???
      class Foo { type X }
      val foo = new Foo { type X = String }
    }

    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree: TypeTree =>
            println(TastyPrinter.stringOf(tree.tpe))
            println()
          case tree: TypeBoundsTree =>
            println(TastyPrinter.stringOf(tree.tpe))
            println()
          case _ =>
        }
        traverseChildren(tree)
      }
    }

    val (tree, ctx) = q.toTasty
    traverser.traverse(tree)(ctx)

  }
}
