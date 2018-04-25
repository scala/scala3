
import dotty.tools.dotc.quoted.Toolbox._

import scala.quoted._
import scala.tasty.util.{TastyPrinter, TreeTraverser}
import scala.tasty.trees._

object Test {
  def main(args: Array[String]): Unit = {
    def test(tpe: Type[_]) = {
      val (tasty, ctx) = tpe.toTasty
      println(TastyPrinter.stringOf(tasty)(ctx))
      println(TastyPrinter.stringOf(tasty.tpe)(ctx))
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
