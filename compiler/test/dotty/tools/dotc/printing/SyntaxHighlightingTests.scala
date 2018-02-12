package dotty.tools.dotc.printing

import dotty.tools.io.JFile
import org.junit.Test
import org.junit.Assert.fail

import scala.io.Source

/** Runs all tests contained in `compiler/test-resources/printing/`
  * To check test cases, you can use "cat" or "less -r" from bash*/
class SyntaxHighlightingTests {

  private def scripts(path: String): Array[JFile] = {
    val dir = new JFile(getClass.getResource(path).getPath)
    assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
    dir.listFiles
  }

  private def testFile(f: JFile): Unit = {
    val linesIt = Source.fromFile(f).getLines()
    val input = linesIt.takeWhile(_ != "result:").mkString("\n")
    val expectedOutput = linesIt.mkString("\n")
    val actualOutput = SyntaxHighlighting(input).mkString

    if (expectedOutput != actualOutput) {
      println("Expected output:")
      println(expectedOutput)
      println("Actual output:")
      println(actualOutput)

      fail(s"Error in file $f, expected output did not match actual")
    }
  }

  @Test def syntaxHighlight = scripts("/printing").foreach(testFile)
}
