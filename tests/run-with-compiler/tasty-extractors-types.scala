
import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.quoted._

import scala.tasty.util.TreeTraverser
import scala.tasty.trees._

object Test {
  def main(args: Array[String]): Unit = {
    def test(tpe: Type[_]) = {
      val tasty = tpe.toTasty
      println(tasty)
      println(tasty.tpe)
      println()

    }
    val i = '[Int]

    val tests = List(
      '[Int],
      '[List[String]],
      '[Map[String, Int]],
      '[Map[String, ~i]],
    )
    tests.foreach(test)
  }
}
