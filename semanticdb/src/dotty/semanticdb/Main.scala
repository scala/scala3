package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.util.TreeTraverser
import scala.tasty.file._

object Main {
  def main(args: Array[String]): Unit = {
    val extraClasspath = "." // TODO allow to set it from the args with -classpath XYZ
    val classes = args.toList
    if (args.isEmpty) {
      println("Dotty Semantic DB: No classes where passed as argument")
    } else {
      println("Running Dotty Semantic DB on: " + args.mkString(" "))
      ConsumeTasty(extraClasspath, classes, new SemanticdbConsumer)
    }
  }
}
