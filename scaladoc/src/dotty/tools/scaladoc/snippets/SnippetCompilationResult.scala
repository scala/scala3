package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }

case class SnippetCompilerMessage(line: Int, column: Int, sourceLine: String, message: String, level: MessageLevel)

case class SnippetCompilationResult(result: Option[AbstractFile], messages: Seq[SnippetCompilerMessage]):
  def getSummary: String = messages.map(m => s"At ${m.line}:${m.column}:\n${m.sourceLine}${m.level.text}: ${m.message}").mkString("\n")


enum MessageLevel(val text: String):
  case Info extends MessageLevel("Info")
  case Warning extends MessageLevel("Warning")
  case Error extends MessageLevel("Error")

