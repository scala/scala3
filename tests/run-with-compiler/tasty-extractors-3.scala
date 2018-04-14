
import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.quoted._

import scala.tasty.util.TreeTraverser
import scala.tasty._
import scala.tasty.trees._

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
      override def traverse(tree: Tree): Unit = {
        tree match {
          case tree: TypeTree =>
            println(tree.tpe)
            println()
          case tree: TypeBoundsTree =>
            println(tree.tpe)
            println()
          case _ =>
        }
        traverseChildren(tree)
      }
    }

    traverser.traverse(q.toTasty)

  }
}
