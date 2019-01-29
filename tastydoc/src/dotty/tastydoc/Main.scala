package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file._

object Main {
  def main(args: Array[String]): Unit = {
    val extraClasspath = "." // TODO allow to set it from the args with -classpath XYZ
    val classes = args.toList
    if (args.isEmpty) {
      println("Dotty Tastydoc: No classes where passed as argument")
    } else {
      println("Running Dotty Tastydoc on: " + args.mkString(" "))
      ConsumeTasty(extraClasspath, classes, new TastydocConsumer)
    }
  }
}
