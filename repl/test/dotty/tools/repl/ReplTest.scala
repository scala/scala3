package dotty.tools
package repl

import scala.language.unsafeNulls

import vulpix.TestConfiguration
import vulpix.FileDiff
import dotty.tools.ToolName
import dotty.tools.readLines
import dotty.tools.toolArgsFor

import java.lang.System.{lineSeparator => EOL}
import java.io.{ByteArrayOutputStream, File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ArrayBuffer

import dotc.core.Contexts.Context
import dotc.reporting.MessageRendering
import org.junit.{After, Before}
import org.junit.Assert._

class ReplTest(options: Array[String] = ReplTest.defaultOptions, out: ByteArrayOutputStream = new ByteArrayOutputStream)
extends ReplDriver(options, new PrintStream(out, true, StandardCharsets.UTF_8.name)) with MessageRendering:
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

  def initially[A](op: State ?=> A): A = op(using initialState)

  def contextually[A](op: Context ?=> A): A = op(using initialState.context)

  /** Returns the `(<instance completions>, <companion completions>)`*/
  def tabComplete(src: String)(implicit state: State): List[String] =
    completions(src.length, src, state).map(_.label).sorted.distinct

  extension [A](state: State)
    infix def andThen(op: State ?=> A): A = op(using state)

  def testFiles(it: Array[JFile]): Unit =
    val errors = it.collect(testFile.unlift)
    if errors.length > 0 then
      fail(errors.mkString("\n"))

  def testScript(name: => String, str: String): Unit =
    testScript(name, str.linesIterator.toList).foreach(fail)

  private def testFile(f: JFile): Option[String] =
    testScript(f.toString, readLines(f), Some(f))

  /** Returns failures: None if all is well, Some for an error */
  private def testScript(name: => String, lines: List[String], scriptFile: Option[JFile] = None): Option[String] = {
    val prompt = "scala>"

    def evaluate(state: State, input: String) =
      try {
        val nstate = run(input.drop(prompt.length))(using state)
        val out = input + EOL + storedOutput()
        (out, nstate)
      }
      catch {
        case ex: Throwable =>
          System.err.println(s"failed while running script: $name, on:\n$input")
          throw ex
      }

    def filterEmpties(line: String): List[String] =
      line.replaceAll("""(?m)\s+$""", "") match {
        case "" => Nil
        case nonEmptyLine => nonEmptyLine :: Nil
      }
    def nonBlank(line: String): Boolean = line.exists(!Character.isWhitespace(_))

    val expectedOutput = lines.filter(nonBlank)
    val actualOutput = {
      val opts = toolArgsFor(ToolName.Scalac, scriptFile.map(_.toString))(lines.take(1))
      val (optsLine, inputLines) = if opts.isEmpty then ("", lines) else (lines.head, lines.drop(1))
      resetToInitial(opts)

      assert(inputLines.head.startsWith(prompt),
        s"""Each script must start with the prompt: "$prompt"""")
      val inputRes = inputLines.filter(_.startsWith(prompt))

      val buf = new ArrayBuffer[String]
      inputRes.foldLeft(initialState) { (state, input) =>
        val (out, nstate) = evaluate(state, input)
        out.linesIterator.foreach(buf.append)
        nstate
      }
      // We run the repl with `-Ydebug` to not use the fallback if there's a class load exception so we can notice these,
      // but as a consequence we get debug messages; ignore the stale symbol one, whose contents include symbol IDs
      // that depend on the exact compiler.
      (optsLine :: buf.toList).filter(nonBlank).filter(!_.startsWith("stale symbol;"))
    }

    if FileDiff.matches(actualOutput, expectedOutput) then None
    else
      // Some tests aren't file-based but just pass a string, so can't update anything then
      // Also the files here are the copies in target/ not the original, so you need to vimdiff/mv them...
      if dotty.Properties.testsUpdateCheckfile && scriptFile.isDefined then
        val checkFile = scriptFile.get
        FileDiff.dump(checkFile.toPath.toString, actualOutput)
        println(s"Wrote updated script file to $checkFile")
        None
      else
        println(dotty.tools.dotc.util.DiffUtil.mkColoredHorizontalLineDiff(actualOutput.mkString(EOL), expectedOutput.mkString(EOL)))

        Some(s"Error in script $name, expected output did not match actual")
    end if
  }

object ReplTest:
  // Because we test REPL features like completion,
  // we don't want other test stuff to get in the way,
  // e.g., "Pred" should complete to "Predef" and not "PredefTest" just because some dependency has a test named like that
  private val classpath =
    System.getProperty("java.class.path")
      .split(JFile.pathSeparator)
      .filter(!_.contains("test-classes"))
      .mkString(JFile.pathSeparator)

  def createOptions(extraClasspath: String*): Array[String] =
    Array("-color:never", "-pagewidth", "80", "-Ydebug",
          "-classpath", classpath + JFile.pathSeparator + extraClasspath.mkString(JFile.pathSeparator))

  val defaultOptions: Array[String] = createOptions()
