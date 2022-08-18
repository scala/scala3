package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream

case class WrappedSnippet(snippet: String, outerLineOffset: Int, outerColumnOffset: Int, innerLineOffset: Int, innerColumnOffset: Int)

object WrappedSnippet:

  val indent: Int = 2

  def apply(
    str: String,
    packageName: Option[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    ps.startHide()
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    ps.println("object Snippet {")
    ps.endHide()

    str.split('\n').foreach(ps.printlnWithIndent(indent, _))

    ps.startHide()
    ps.println("}")
    ps.endHide()

    WrappedSnippet(
      baos.toString,
      outerLineOffset,
      outerColumnOffset,
      2 + 2 /*Hide tokens*/,
      indent
    )

  extension (ps: PrintStream)
    private def printlnWithIndent(indent: Int, str: String) =
      ps.println((" " * indent) + str)
    private def startHide() = ps.println(raw"//{")
    private def endHide() = ps.println(raw"//}")


