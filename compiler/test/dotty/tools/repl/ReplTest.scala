package dotty.tools
package repl

import vulpix.TestConfiguration
import vulpix.FileDiff

import java.lang.System.{lineSeparator => EOL}
import java.io.{ByteArrayOutputStream, File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ArrayBuffer

import dotty.tools.dotc.reporting.MessageRendering
import org.junit.{After, Before}
import org.junit.Assert._

class ReplTest(options: Array[String] = ReplTest.defaultOptions, out: ByteArrayOutputStream = new ByteArrayOutputStream)
extends ReplDriver(options, new PrintStream(out, true, StandardCharsets.UTF_8.name)) with MessageRendering {
  /** Get the stored output from `out`, resetting the buffer */
  def storedOutput(): String = {
    val output = stripColor(out.toString(StandardCharsets.UTF_8.name))
    out.reset()
    output
  }

  /** Make sure the context is new before each test */
  @Before def init(): Unit =
    resetToInitial()

  /** Reset the stored output */
  @After def cleanup: Unit =
    storedOutput()

  def fromInitialState[A](op: State => A): A =
    op(initialState)

  extension [A](state: State)
    def andThen(op: State => A): A = op(state)

  def testFile(f: JFile): Unit = {
    val prompt = "scala>"

    def evaluate(state: State, input: String) =
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

    val expectedOutput = readLines(f).flatMap(filterEmpties)
    val actualOutput = {
      resetToInitial()

      val lines = readLines(f)
      assert(lines.head.startsWith(prompt),
        s"""Each file has to start with the prompt: "$prompt"""")
      val inputRes = lines.filter(_.startsWith(prompt))

      val buf = new ArrayBuffer[String]
      inputRes.foldLeft(initialState) { (state, input) =>
        val (out, nstate) = evaluate(state, input)
        out.linesIterator.foreach(buf.append)

        assert(out.endsWith("\n"),
               s"Expected output of $input to end with newline")

        nstate
      }
      buf.toList.flatMap(filterEmpties)
    }

    if !FileDiff.matches(actualOutput, expectedOutput) then
      println("expected =========>")
      println(expectedOutput.mkString(EOL))
      println("actual ===========>")
      println(actualOutput.mkString(EOL))

      fail(s"Error in file $f, expected output did not match actual")
    end if
  }
}

object ReplTest:
  val commonOptions = Array("-color:never", "-language:experimental.erasedDefinitions", "-pagewidth", "80")
  val defaultOptions = commonOptions ++ Array("-classpath", TestConfiguration.basicClasspath)
  lazy val withStagingOptions = commonOptions ++ Array("-classpath", TestConfiguration.withStagingClasspath)
