package dotty.tools.dotc.printing

import java.io.PrintWriter

import dotty.tools.io.JFile
import org.junit.Assert.fail
import org.junit.Test

import scala.io.Source

/** Runs all tests contained in `compiler/test-resources/printing/`
  * To check test cases, you can use "cat" or "less -r" from bash
  * To generate test files you can call the generateTestFile method*/
class SyntaxHighlightingTests {

  private def scripts(path: String): Array[JFile] = {
    val dir = new JFile(getClass.getResource(path).getPath)
    assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
    dir.listFiles
  }

  private def testFile(f: JFile): Unit = {
    val lines = Source.fromFile(f).getLines()
    val input = lines.takeWhile(_ != "result:").mkString("\n")
    val expectedOutput = lines.mkString("\n")
    val actualOutput = SyntaxHighlighting(input).mkString

    if (expectedOutput != actualOutput) {
      println("Expected output:")
      println(expectedOutput)
      println("Actual output:")
      println(actualOutput)

      // Call generateTestFile when you want to update a test file
      // or generate a test from a scala source file
      // if (f.getName == "nameOfFileToConvertToATest") generateTestFile()

      fail(s"Error in file $f, expected output did not match actual")
    }

    /** Writes `input` and `actualOutput` to the current test file*/
    def generateTestFile(): Unit = {
      val path = "compiler/test-resources/printing/" + f.getName
      new PrintWriter(path) {
        write(input + "\nresult:\n" + actualOutput)
        close()
      }
      println(s"Test file for ${f.getName} has been generated")
    }
  }

  @Test def syntaxHighlight = scripts("/printing").foreach(testFile)
}
