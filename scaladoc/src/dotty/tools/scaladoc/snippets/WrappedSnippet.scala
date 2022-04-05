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
    classInfos: Seq[SnippetCompilerData.ClassInfo],
    imports: List[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
    isMacro: Boolean = false,
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.startHide()
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    imports.foreach(i => ps.println(s"import $i"))
    val nonEmptyClassInfos = if classInfos.isEmpty then Seq(SnippetCompilerData.ClassInfo(None, Nil, None)) else classInfos

    if isMacro then
      ps.println("object Snippet {")
    else
      nonEmptyClassInfos.zipWithIndex.foreach { (info, i) =>
        ps.printlnWithIndent(indent * i, s"trait Snippet$i${info.generics.getOrElse("")} { ${info.tpe.fold("")(cn => s"self: $cn =>")}")
        info.names.foreach{ name =>
          ps.printlnWithIndent(indent * i + indent, s"val $name = self")
        }
      }

    ps.endHide()
    val (indentsMade, createdVals) = if isMacro then
      (1, 1)
    else
      (nonEmptyClassInfos.size, nonEmptyClassInfos.flatMap(_.names).size)

    str.split('\n').foreach(ps.printlnWithIndent(indentsMade * indent, _))
    ps.startHide()
    (0 to indentsMade -1).reverse.foreach( i => ps.printlnWithIndent(i * indent, "}"))
    ps.endHide()

    WrappedSnippet(
      baos.toString,
      outerLineOffset,
      outerColumnOffset,
      indentsMade + imports.length + createdVals + packageName.size + 2 /*Hide tokens*/,
      indentsMade * indent
    )

  extension (ps: PrintStream)
    private def printlnWithIndent(indent: Int, str: String) =
      ps.println((" " * indent) + str)
    private def startHide() = ps.println(raw"//{")
    private def endHide() = ps.println(raw"//}")


