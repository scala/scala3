package dotty.tools
package repl

import java.io.{File => JFile}
import java.lang.System.{lineSeparator => EOL}

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import dotc.reporting.MessageRendering

/** Runs all tests contained in `compiler/test-resources/repl/` */
class ScriptedTests extends ReplTest with MessageRendering {

  private def scripts(path: String): Array[JFile] = {
    val dir = new JFile(getClass.getResource(path).getPath)
    assert(dir.exists && dir.isDirectory, "Couldn't load scripts dir")
    dir.listFiles
  }

  private def testFile(f: JFile): Unit = {
    val prompt = "scala>"
    val lines = Source.fromFile(f, "UTF-8").getLines().buffered

    assert(lines.head.startsWith(prompt),
           s"""Each file has to start with the prompt: "$prompt"""")

    def extractInputs(prompt: String): List[String] = {
      val input = lines.next()

      if (!input.startsWith(prompt)) extractInputs(prompt)
      else if (lines.hasNext) {
        // read lines and strip trailing whitespace:
        while (lines.hasNext && !lines.head.startsWith(prompt))
          lines.next()

        input :: { if (lines.hasNext) extractInputs(prompt) else Nil }
      }
      else Nil
    }

    def evaluate(state: State, input: String, prompt: String) =
      try {
        val nstate = run(input.drop(prompt.length))(state)
        val out = input + EOL + storedOutput()
        (out, nstate)
      }
      catch {
        case ex: Throwable =>
          System.err.println(s"failed while running script: $f, on:\n$input")
          throw ex
      }

    def filterEmpties(line: String): List[String] =
      line.replaceAll("""(?m)\s+$""", "") match {
        case "" => Nil
        case nonEmptyLine => nonEmptyLine :: Nil
      }

    def printOutput(prefix: String, output: String): Unit = {
      val bytes = output.getBytes
      println(prefix+
        " (#chars="+output.length+
        ", #CR="+(bytes.count(_ == 13))+
        ", #LF="+(bytes.count(_ == 10))+
        ") ========>"+EOL+
        output.replaceAll("\r", "<CR>").replaceAll("\n", "<LF>"+EOL))
    }
 
    val expectedOutput =
      Source.fromFile(f, "UTF-8").getLines().flatMap(filterEmpties).mkString(EOL)
    val actualOutput = {
      resetToInitial()
      val inputRes = extractInputs(prompt)
      val buf = new ArrayBuffer[String]
      inputRes.foldLeft(initialState) { (state, input) =>
        val (out, nstate) = evaluate(state, input, prompt)
        buf.append(out)

        assert(out.endsWith("\n"),
               s"Expected output of $input to end with newline")

        nstate
      }
      buf.flatMap(filterEmpties).mkString(EOL)
    }

    if (expectedOutput != actualOutput) {
      printOutput("expected", expectedOutput)
      printOutput("actual", actualOutput)

      fail(s"Error in file $f, expected output did not match actual")
    }
  }

  @Test def replTests = scripts("/repl").foreach(testFile)

  @Test def typePrinterTests = scripts("/type-printer").foreach(testFile)
}
