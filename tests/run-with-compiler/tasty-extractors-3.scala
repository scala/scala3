
import scala.quoted._
import scala.tasty.typetrees._
import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.tasty.patterns.{CaseDef, Pattern}
import scala.tasty.statements.TopLevelStatement
import scala.tasty.typetrees.TypeTree
import scala.tasty.util.TreeTraverser

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
      override def traverse(arg: MaybeTypeTree): Unit = {
        println(arg.tpe)
        println()
        traverseChildren(arg)
      }
      override def traverse(tree: TopLevelStatement): Unit = traverseChildren(tree)
      override def traverse(tree: CaseDef): Unit = traverseChildren(tree)
      override def traverse(tree: Pattern): Unit = traverseChildren(tree)
    }

    traverser.traverse(q.toTasty)

  }
}
