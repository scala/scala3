package test.transform

import java.io._
import scala.io.Source._
import scala.reflect.io.Directory
import org.junit.Test

import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.ConsoleReporter

class PatmatExhaustivityTest {
  val testsDir = "./tests/patmat"
  val options = "-Ystop-after:splitter"  // no need for code generation
                                         // patmatexhaust-huge.scala crash compiler

  private def compileFile(file: File) = {
    val stringBuffer = new StringWriter()
    val reporter = new ConsoleReporter(writer = new PrintWriter(stringBuffer))

    try {
      Main.process(Array(file.getPath, options), reporter, null)
    } catch {
      case e: Throwable =>
        println(s"Compile $file exception:")
        e.printStackTrace()
    }

    val actual = stringBuffer.toString.trim
    val checkFilePath = file.getAbsolutePath.stripSuffix(".scala") + ".check"
    val checkContent =
      if (new File(checkFilePath).exists)
        fromFile(checkFilePath).getLines.mkString("\n").trim
      else ""

    (file, checkContent, actual)
  }

  @Test def patmatExhaustivity: Unit = {
    val res = Directory(testsDir).deepFiles.toList.filter(_.extension == "scala").map { f =>
      compileFile(f.jfile)
    }

    val failed = res.filter { case (_, expected, actual) => expected != actual }
    val ignored = Directory(testsDir).deepFiles.toList.filter(_.extension == "ignore")

    failed.foreach { case (file, expected, actual) =>
      println(s"\n----------------- incorrect output for $file --------------\n" +
        s"Expected:\n-------\n$expected\n\nActual\n----------\n$actual\n"
      )
    }

    val msg = s"Total: ${res.length + ignored.length}, Failed: ${failed.length}, Ignored: ${ignored.length}"

    assert(failed.length == 0, msg)

    println(msg)
  }
}
