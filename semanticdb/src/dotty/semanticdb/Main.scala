package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file._
import scala.NotImplementedError

import dotty.tools.dotc.Driver

object Main {
  def parseArguments(args: Array[String]): (Map[String, String], Boolean) = {
    def nextArgument(optionMap: Map[String, String], args: List[String]): Map[String, String] = args match {
      case "--out" :: file :: tail => nextArgument(optionMap + ("outfile" -> file), tail)
      case "-o" :: file :: tail => nextArgument(optionMap + ("outfile" -> file), tail)
      case "--help" :: tail => nextArgument(optionMap + ("help" -> ""), tail)
      case "-h" :: tail => nextArgument(optionMap + ("help" -> ""), tail)
      case "--temp" :: folder :: tail => nextArgument(optionMap + ("temp" -> folder), tail)
      case "-t" :: folder :: tail => nextArgument(optionMap + ("temp" -> folder), tail)
      case file :: tail => nextArgument(optionMap + ("input" -> file), tail)
      case Nil => optionMap
    }

    val help = """Usage semanticdb [options] [file]
    |Generate semanticdb's information related to the source file [file]
    |Options are:
    | -h,--help                    Show help
    | -o <file>, --out <file>      Place the output into <file>
    | -t <folder>, --temp <folder> Use <folder> as the temp directory to store build artifacts
    """.stripMargin

    var optionMap = nextArgument(Map(), args.toList)
    if (optionMap.contains("help") || !optionMap.contains("input")) {
      println(help)
      return (optionMap, false)
    } else {
      if (!optionMap.contains("temp")) {
        optionMap += ("temp" -> "/home/pierre/epfl/semester/dotty/semanticdb/testClass/")
      }
      val driver = new Driver
      val compilerParams : List[String] =
      optionMap("input") ::
      "-Yno-inline" ::
      "-d" :: optionMap("temp") ::
      "-classpath" :: "/home/pierre/epfl/semester/dotty/library/../out/bootstrap/dotty-library-bootstrapped/scala-0.12/dotty-library_0.12-0.12.0-bin-SNAPSHOT.jar" ::
      Nil

      val foo = driver.process(compilerParams.toArray)
      return (optionMap, true)
    }
  }


  def main(args: Array[String]): Unit = {
    val extraClasspath = "." // TODO allow to set it from the args with -classpath XYZ
    val classes = args.toList
    val (optionMap, canTreat) = parseArguments(args)
    if (args.isEmpty) {
      println("Dotty Semantic DB: No classes where passed as argument")
    } else {
      println("Running Dotty Semantic DB on: " + args.mkString(" "))
      //ConsumeTasty(extraClasspath, classes, new SemanticdbConsumer())
    }
  }
}
