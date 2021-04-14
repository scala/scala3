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
    classInfos: Seq[SnippetCompilerData.ClassInfo],
    imports: List[String],
    lineOffset: Int,
    columnOffset: Int
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    imports.foreach(i => ps.println(s"import $i"))
    classInfos.zipWithIndex.foreach { (info, i) =>
      ps.printlnWithIndent(2 * i, s"trait Snippet$i${info.generics.getOrElse("")} { ${info.tpe.fold("")(cn => s"self: $cn =>")}")
      info.names.foreach{ name =>
        ps.printlnWithIndent(2 * i + 2, s"val $name = self")
      }
    }
    str.split('\n').foreach(ps.printlnWithIndent(classInfos.size * 2, _))
    (0 to classInfos.size -1).reverse.foreach( i => ps.printlnWithIndent(i * 2, "}"))
    WrappedSnippet(baos.toString, lineOffset, columnOffset, classInfos.size + classInfos.flatMap(_.names).size, classInfos.size * 2 + 2)

  extension (ps: PrintStream) private def printlnWithIndent(indent: Int, str: String) =
    ps.println((" " * indent) + str)


