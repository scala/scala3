package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }

case class Position(line: Int, column: Int, sourceLine: String)

case class SnippetCompilerMessage(position: Option[Position], message: String, level: MessageLevel)

case class SnippetCompilationResult(
  isSuccessful: Boolean,
  result: Option[AbstractFile],
  messages: Seq[SnippetCompilerMessage]
):
  def getSummary: String =
    messages.map(m =>
      m.position.fold(
        s"${m.level.text}: ${m.message}"
      )(pos =>
        s"At ${pos.line}:${pos.column}:\n${pos.sourceLine}${m.level.text}: ${m.message}"
      )
    ).mkString("\n")


enum MessageLevel(val text: String):
  case Info extends MessageLevel("Info")
  case Warning extends MessageLevel("Warning")
  case Error extends MessageLevel("Error")
  case Debug extends MessageLevel("Debug")

