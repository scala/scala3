package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream

class SnippetWrapper:
  extension (ps: PrintStream) def printlnWithIndent(indent: Int, str: String) =
    ps.println((" " * indent) + str)
  def wrap(str: String): String =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    ps.println("package snippets")
    ps.println("object Snippet {")
    str.split('\n').foreach(ps.printlnWithIndent(2, _))
    ps.println("}")
    baos.toString

object SnippetWrapper:
  val lineOffset = 2
  val columnOffset = 2