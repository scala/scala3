package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }

case class Position(line: Int, column: Int, sourceLine: String, relativeLine: Int)

case class SnippetCompilerMessage(position: Option[Position], message: String, level: MessageLevel):
  def getSummary: String =
    position.fold(s"${level.text}: ${message}") { pos =>
      s"At ${pos.line}:${pos.column}:\n${pos.sourceLine}${level.text}: ${message}"
    }

case class SnippetCompilationResult(
  wrappedSnippet: WrappedSnippet,
  isSuccessful: Boolean,
  result: Option[AbstractFile],
  messages: Seq[SnippetCompilerMessage]
):
  def getSummary: String = messages.map(_.getSummary).mkString("\n")

enum MessageLevel(val text: String):
  case Info extends MessageLevel("Info")
  case Warning extends MessageLevel("Warning")
  case Error extends MessageLevel("Error")
  case Debug extends MessageLevel("Debug")

