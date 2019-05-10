package dotty.tastydoc

import scala.tasty.Reflection
import scala.tasty.file._

object Main {
  def main(args: Array[String]): Unit = {

    val (extraClasspath, classes) = {
      val idx = args.indexOf("-classpath")
      if(idx >= 0 && args.size > idx + 1){
        (args(idx + 1), args.filter(_!=(args(idx + 1))).toList)
      }else{
        (".", args.toList)
      }
    }

    if (args.isEmpty) {
      println("Dotty Tastydoc: No classes where passed as argument")
    } else {
      println("Running Dotty Tastydoc on: " + args.mkString(" "))
      ConsumeTasty(extraClasspath, classes, new TastydocConsumer)
    }
  }
}
