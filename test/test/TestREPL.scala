package test

import dotty.tools.dotc.repl._
import dotty.tools.dotc.core.Contexts.Context
import collection.mutable
import java.io.StringWriter


class TestREPL(script: String) extends REPL {

  private val prompt             = "scala> "
  private val continuationPrompt = "     | "

  private val out = new StringWriter()
  override val output = new NewLinePrintWriter(out)

  override def input(implicit ctx: Context) = new InteractiveReader {
    val lines = script.lines
    def readLine(prompt: String): String = {
      val line = lines.next
      if (line.startsWith(prompt) || line.startsWith(continuationPrompt)) {
        output.println(line)
        line.drop(prompt.length)
      }
      else readLine(prompt)
    }
    val interactive = false
  }

  def check() = {
    out.close()
    val printed = out.toString
    val transcript = printed.drop(printed.indexOf(prompt))
    if (transcript.toString != script) {
      println("input differs from transcript:")
      println(transcript)
      assert(false)
    }
  }
}