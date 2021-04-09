package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream

case class WrappedSnippet(snippet: String, lineOffset: Int, columnOffset: Int, lineBoilerplate: Int, columnBoilerplate: Int)

object WrappedSnippet:
  private val lineBoilerplate = 2
  private val columnBoilerplate = 2

  def apply(str: String): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println("package snippets")
    ps.println("object Snippet {")
    str.split('\n').foreach(ps.printlnWithIndent(2, _))
    ps.println("}")
    WrappedSnippet(baos.toString, 0, 0, lineBoilerplate, columnBoilerplate)

  def apply(
    str: String,
    packageName: Option[String],
    className: Option[String],
    classGenerics: Option[String],
    imports: List[String],
    lineOffset: Int,
    columnOffset: Int
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    imports.foreach(i => ps.println(s"import $i"))
    ps.println(s"trait Snippet${classGenerics.getOrElse("")} { ${className.fold("")(cn => s"self: $cn =>")}")
    str.split('\n').foreach(ps.printlnWithIndent(2, _))
    ps.println("}")
    WrappedSnippet(baos.toString, lineOffset, columnOffset, lineBoilerplate, columnBoilerplate)

  extension (ps: PrintStream) private def printlnWithIndent(indent: Int, str: String) =
    ps.println((" " * indent) + str)


