package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream

class SnippetWrapper:
  extension (ps: PrintStream) private def printlnWithIndent(indent: Int, str: String) =
    ps.println((" " * indent) + str)
  def wrap(str: String): String =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println("package snippets")
    ps.println("object Snippet {")
    str.split('\n').foreach(ps.printlnWithIndent(2, _))
    ps.println("}")
    baos.toString

  def wrap(str:String, packageName: Option[String], className: Option[String], classGenerics: Option[String], imports: List[String]) =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    imports.foreach(i => ps.println(s"import $i"))
    ps.println(s"trait Snippet${classGenerics.getOrElse("")} { ${className.fold("")(cn => s"self: $cn =>")}")
    str.split('\n').foreach(ps.printlnWithIndent(2, _))
    ps.println("}")
    baos.toString

object SnippetWrapper:
  private val lineOffset = 2
  private val columnOffset = 2