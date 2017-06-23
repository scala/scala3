package dotty
package tools.dotc
package repl

import core.Contexts.Context
import collection.mutable
import java.io.{StringWriter, PrintStream}
import dotty.tools.io.{ PlainFile, Directory }
import org.junit.Test

/** A subclass of REPL used for testing.
 *  It takes a transcript of a REPL session in `script`. The transcript
 *  starts with the first input prompt `scala> ` and ends with `scala> :quit` and a newline.
 *  Invoking `process()` on the `TestREPL` runs all input lines and
 *  collects then interleaved with REPL output in a string writer `out`.
 *  Invoking `check()` checks that the collected output matches the original
 *  `script`.
 */
class TestREPL(script: String) extends REPL {

  private val out = new StringWriter()

  override lazy val config = new REPL.Config {
    override val output = new NewLinePrintWriter(out)

    override def context(ctx: Context) = {
      val fresh = ctx.fresh
      fresh.setSetting(ctx.settings.color, "never")
      fresh.setSetting(ctx.settings.classpath, Jars.dottyReplDeps.mkString(":"))
      fresh.initialize()(fresh)
      fresh
    }

    override def input(in: Interpreter)(implicit ctx: Context) = new InteractiveReader {
      val lines = script.lines.buffered
      def readLine(prompt: String): String = {
        val line = lines.next()
        val buf = new StringBuilder
        if (line.startsWith(prompt)) {
          output.println(line)
          buf append line.drop(prompt.length)
          while (lines.hasNext && lines.head.startsWith(continuationPrompt)) {
            val continued = lines.next()
            output.println(continued)
            buf append System.lineSeparator()
            buf append continued.drop(continuationPrompt.length)
          }
          buf.toString
        }
        else readLine(prompt)
      }
      val interactive = false
    }
  }

  def check() = {
    out.close()
    val printed = out.toString
    val transcript = printed.drop(printed.indexOf(config.prompt))
    if (transcript.toString.lines.toList != script.lines.toList) {
      println("input differs from transcript (copy is repl.transcript):")
      println(transcript)
      val s = new PrintStream("repl.transcript")
      s.print(transcript)
      s.close()
      assert(false)
    }
  }
}

class REPLTests {
  def replFile(prefix: String, fileName: String): Unit = {
    val path = s"$prefix$fileName"
    val f = new PlainFile(path)
    val repl = new TestREPL(new String(f.toCharArray))
    repl.process(Array[String]())
    repl.check()
  }

  def replFiles(path: String): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(_ endsWith ".check")
    for (name <- fileNames) {
      println(s"testing $path$name")
      replFile(path, name)
    }
  }

  @Test def replAll = replFiles("../tests/repl/")
}
