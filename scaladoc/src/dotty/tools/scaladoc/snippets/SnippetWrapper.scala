package dotty.tools.scaladoc
package snippets

class SnippetWrapper:
  def wrap(str: String): String = s"""
  |package snippets
  |object Snippet {
  |  $str
  |}
  |""".stripMargin

object SnippetWrapper:
  val lineOffset = 2
  val columnOffset = 2