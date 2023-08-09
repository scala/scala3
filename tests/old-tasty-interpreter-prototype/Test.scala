import java.io.{ByteArrayOutputStream, File, PrintStream}

import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.DiffUtil
import dotty.tools.io.Path

import scala.io.Source
import scala.util.Using
import scala.tasty.interpreter.TastyInterpreter
import scala.tasty.inspector.TastyInspector

object Test {

  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList

    val actualOutput = interpret(allTastyFiles.filter(x => x.contains("IntepretedMain") || x.contains("InterpretedBar")))
    val expectedOutput =
      """42
        |
        |Hello
        |
        |42
        |
        |43
        |
        |if
        |
        |5
        |4
        |3
        |2
        |1
        |
        |42
        |
        |55
        |precompiledModule
        |55
        |56
        |57
        |58
        |59
        |60
        |61
        |62
        |63
        |true
        |false
        |64
        |65
        |66
        |67
        |68
        |""".stripMargin

    assert(expectedOutput == actualOutput,
      "\n>>>>>>>>>>>>>>>>>>\n" +
      DiffUtil.mkColoredCodeDiff(actualOutput, expectedOutput, true) +
      "<<<<<<<<<<<<<<<<<<"
    )

    compileAndInterpret("HelloWorld.scala")
    compileAndInterpret("nullInstanceEval.scala")
    compileAndInterpret("t3327.scala")
//    compileAndInterpret("t5614.scala")
//    compileAndInterpret("t4054.scala")
//    compileAndInterpret("sort.scala")
//    compileAndInterpret("t0607.scala")
//    compileAndInterpret("i4073b.scala")
//    compileAndInterpret("i4430.scala")
//    compileAndInterpret("nullAsInstanceOf.scala")
//    compileAndInterpret("classof.scala")
//    compileAndInterpret("null-hash.scala")
//    compileAndInterpret("i3518.scala")
//    compileAndInterpret("withIndex.scala")
//    compileAndInterpret("unboxingBug.scala")
//    compileAndInterpret("traitInit.scala")
  }

  def compileAndInterpret(testFileName: String) = {
    val reproter = new Reporter {
      def doReport(dia: Diagnostic)(implicit ctx: Contexts.Context): Unit = println(dia)
    }
    val out = java.nio.file.Paths.get("out/interpreted")
    if (!java.nio.file.Files.exists(out))
      java.nio.file.Files.createDirectory(out)

    val filePath = "tests" + File.separator + "run" + File.separator + testFileName
    dotty.tools.dotc.Main.process(Array("-classpath", System.getProperty("java.class.path"), "-d", out.toString, filePath), reproter)

    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val allTastyFiles = dotty.tools.io.Path(out).walkFilter(_.extension == "tasty").map(_.toString).toList

    val actualOutput = interpret(allTastyFiles.filter(_.endsWith("Test.tasty")))

    val checkFile = java.nio.file.Paths.get("tests/run/" + testFileName.stripSuffix(".scala") + ".check")
    if (java.nio.file.Files.exists(checkFile)) {
      val expectedOutput = Using(Source.fromFile(checkFile.toFile))(_.getLines().mkString("", "\n", "\n")).get

      assert(expectedOutput == actualOutput,
        "\n>>>>>>>>>>>>>>>>>>\n" +
          DiffUtil.mkColoredCodeDiff(actualOutput, expectedOutput, true) +
          "<<<<<<<<<<<<<<<<<<"
      )
    }
  }

  def interpret(interpretedClasses: List[String]): String = {
    val ps = new ByteArrayOutputStream()
    try scala.Console.withOut(ps) {
      TastyInspector.inspectTastyFiles(interpretedClasses.toList)(new TastyInterpreter)
    } catch {
      case e: Throwable => throw new Exception(ps.toString, e)
    }
    ps.toString
  }
}
