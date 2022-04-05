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
    imports: List[String],
    outerLineOffset: Int,
    outerColumnOffset: Int
  ): WrappedSnippet =
    apply(str, packageName, Nil, imports, outerLineOffset, outerColumnOffset)

  def apply(
    str: String,
    packageName: Option[String],
    classInfos: Seq[SnippetCompilerData.ClassInfo],
    imports: List[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
    usingQuotes: Boolean = false
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.startHide()
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    imports.foreach(i => ps.println(s"import $i"))
    val nestLevels = if classInfos.isEmpty then 1 else classInfos.length + (if usingQuotes then 1 else 0)
    if classInfos.isEmpty then 
      ps.println("object Snippet {")
    else
      classInfos.zipWithIndex.foreach { (info, i) =>
        ps.printlnWithIndent(indent * i, s"trait Snippet$i${info.generics.getOrElse("")} { ${info.tpe.fold("")(cn => s"self: $cn =>")}")
        info.names.foreach{ name =>
          ps.printlnWithIndent(indent * i + indent, s"val $name = self")
        }
      }
    if usingQuotes then 
      ps.printlnWithIndent(classInfos.length * indent, "def f(using Quotes) = {")
    ps.endHide()
    str.split('\n').foreach(ps.printlnWithIndent(nestLevels * indent, _))
    ps.startHide()
    (0 to nestLevels -1).reverse.foreach( i => ps.printlnWithIndent(i * indent, "}"))
    ps.endHide()
    WrappedSnippet(
      baos.toString,
      outerLineOffset,
      outerColumnOffset,
      nestLevels + classInfos.flatMap(_.names).size + packageName.size + 2 /*Hide tokens*/,
      nestLevels * indent
    )

  extension (ps: PrintStream)
    private def printlnWithIndent(indent: Int, str: String) =
      ps.println((" " * indent) + str)
    private def startHide() = ps.println(raw"//{")
    private def endHide() = ps.println(raw"//}")


