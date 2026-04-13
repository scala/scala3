package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import dotty.tools.dotc.config.Feature

case class WrappedSnippet(snippet: String, outerLineOffset: Int, outerColumnOffset: Int, innerLineOffset: Int, innerColumnOffset: Int)

object WrappedSnippet:

  val indent: Int = 2

  /** Matches import lines for global language features that must be at the toplevel. */
  private val globalLanguageImport =
    val names = Feature.globalLanguageImports.map(_.toString.stripPrefix("experimental."))
    raw"import\s+language\s*\.\s*experimental\s*\.\s*(${names.mkString("|")})\b".r.unanchored

  private def isGlobalLanguageImport(line: String): Boolean =
    globalLanguageImport.matches(line.trim)

  def apply(
    str: String,
    packageName: Option[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    val lines = str.split('\n')
    val (globalImports, rest) = lines.partition(isGlobalLanguageImport)

    ps.startHide()
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    globalImports.foreach(ps.println)
    ps.println("object Snippet {")
    ps.endHide()

    rest.foreach(ps.printlnWithIndent(indent, _))

    ps.startHide()
    ps.println("}")
    ps.endHide()

    WrappedSnippet(
      baos.toString,
      outerLineOffset,
      outerColumnOffset,
      2 + globalImports.length + 2 /*Hide tokens*/,
      indent
    )

  extension (ps: PrintStream)
    private def printlnWithIndent(indent: Int, str: String) =
      ps.println((" " * indent) + str)
    private def startHide() = ps.println(raw"//{")
    private def endHide() = ps.println(raw"//}")


