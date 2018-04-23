
import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.quoted._

import scala.tasty.util.TreeTraverser
import scala.tasty.trees._

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
      override def traverse(tree: Tree): Unit = {
        tree match {
          case tree: DefDef =>
            val DefDef(name, _, _, _, _, _) = tree
            println(name)
            println(tree.owner)
            println()
          case tree: ValDef =>
            val ValDef(name, _, _, _) = tree
            println(name)
            println(tree.owner)
            println()
          case _ =>
        }
        traverseChildren(tree)
      }
    }

    traverser.traverse(q.toTasty)

  }
}
