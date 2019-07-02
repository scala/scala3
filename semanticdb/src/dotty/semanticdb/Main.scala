package dotty.semanticdb

import scala.tasty.file._
import scala.NotImplementedError

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.Reporter

import java.io.File
import java.nio.file._

object Main {
  val userHome = System.getProperty("user.home")
  val classpaths =
  userHome + "/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.8.jar" ::
  "out/bootstrap/dotty-library-bootstrapped/scala-0.12/dotty-library_0.12-0.12.0-bin-SNAPSHOT.jar" :: Nil

  val help = """Usage semanticdb [options] [file]
   |Generate semanticdb's information related to the source file [file]
   |Options are:
   | -h,--help                    Show help
   | -o <file>, --out <file>      Place the output into <file> (default: out.semanticdb)
   | -t <folder>, --temp <folder> Use <folder> as the temp directory to store build artifacts
   """.stripMargin

  type CliArgs = Map[String, String]

  def parseArguments(args: Array[String]): Option[CliArgs] = {
    val optRegex = "$-.*".r
    def nextArgument(optionMap: CliArgs, args: List[String]): Option[CliArgs] = args match {
      case "--out" :: file :: tail => nextArgument(optionMap + ("out" -> file), tail)
      case "-o" :: file :: tail => nextArgument(optionMap + ("out" -> file), tail)
      case "--help" :: tail => nextArgument(optionMap + ("help" -> ""), tail)
      case "-h" :: tail => nextArgument(optionMap + ("help" -> ""), tail)
      case "--classpath" :: folder :: tail => nextArgument(optionMap + ("classpath" -> folder), tail)
      case "-c" :: folder :: tail => nextArgument(optionMap + ("classpath" -> folder), tail)
      case optRegex(_) :: _=> None
      case file :: tail => nextArgument(optionMap + ("input" -> file), tail)
      case Nil => Some(optionMap)
    }

    nextArgument(Map(), args.toList) match {
    case Some(args : CliArgs) => {
      var cleanedArgs = args
      cleanedArgs += "out" -> cleanedArgs.getOrElse("out", "out.semanticdb")
      if (cleanedArgs.contains("help") || !cleanedArgs.contains("input")) {
        None
      } else {
        cleanedArgs += "classpath" -> cleanedArgs.getOrElse("classpath", Files.createTempDirectory("semanticdb").toString)
        val tempFolder = new File(cleanedArgs("classpath"));
        if (!tempFolder.exists()){
            tempFolder.mkdir();
        }
        Some(cleanedArgs)
      }
    }
    case None => None
    }
  }

  def compile(cliArgs : CliArgs) : Reporter
 = {
    val driver = new Driver
    val compilerParams : List[String] =
     "-classpath" :: classpaths.mkString(":") ::
     "-Yno-inline" ::
     "-d" :: cliArgs("classpath") ::
     cliArgs("input") ::
    Nil

    driver.process(compilerParams.toArray)
  }


  def main(args: Array[String]): Unit = {
    val extraClasspath = "." // TODO allow to set it from the args with -classpath XYZ
    val classes = args.toList


    parseArguments(args) match {
      case None => println(help)
      case Some(cliArgs) => {
        val reporter = compile(cliArgs)

        if (reporter.hasErrors) {
          println("Compile error:")
          println(reporter)
        } else {
          val scalaFile = Paths.get(cliArgs("input")).toAbsolutePath
          val classNames = Utils.getClassNames(Paths.get(cliArgs("classpath")), scalaFile)
          val sdbconsumer = new SemanticdbConsumer(scalaFile)
          val _ = ConsumeTasty(cliArgs("classpath"), classNames, sdbconsumer)
          val textDocument = sdbconsumer.toSemanticdb()
          val os = Files.newOutputStream(Paths.get(cliArgs("out")))
          try textDocument.writeTo(os)
          finally os.close()
        }
      }
    }
  }
}
