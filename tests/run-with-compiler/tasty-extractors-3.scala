
import scala.quoted._

import scala.tasty.typetrees._

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

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
      override def traverse(arg: TypeTree): Unit = {
        println(arg.tpe)
        println()
        super.traverse(arg)
      }
    }

    traverser.traverse(toTasty(q))

  }



}
