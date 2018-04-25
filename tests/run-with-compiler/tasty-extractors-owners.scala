
import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted._
import scala.tasty.util.{TastyPrinter, TreeTraverser}
import scala.tasty.names.Name
import scala.tasty.trees._
import scala.tasty.Context

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      def foo = {
        def bar = 1
        val bar2 = 2
        bar
      }
      val foo2 = {
        def baz = 3
        val baz2 = 4
        baz
      }
      class A {
        type B = Int
        def b = 5
        val b2 = 6
      }
    }

    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        tree match {
          case tree @ DefDef(name: Name, _, _, _, _, _) =>
            println(TastyPrinter.stringOf(name))
            println(TastyPrinter.stringOf(tree.owner))
            println()
          case tree @ ValDef(name: Name, _, _, _) =>
            println(TastyPrinter.stringOf(name))
            println(TastyPrinter.stringOf(tree.owner))
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
