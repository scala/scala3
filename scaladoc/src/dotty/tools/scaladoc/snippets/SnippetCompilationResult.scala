package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }
import dotty.tools.dotc.util.{ SourcePosition, SrcPos }

case class Position(srcPos: SourcePosition, relativeLine: Int)

case class SnippetCompilerMessage(position: Option[Position], message: String, level: MessageLevel):
  def emit()(using CompilerContext): Unit =
    val pos: SrcPos = position.fold(dotty.tools.dotc.util.NoSourcePosition)(_.srcPos)
    level match
      case MessageLevel.Info => report.log(message, pos)
      case MessageLevel.Warning => report.warning(message, pos)
      case MessageLevel.Error => report.error(message, pos)
      case MessageLevel.Debug => report.log(message, pos)

case class SnippetCompilationResult(
  wrappedSnippet: WrappedSnippet,
  isSuccessful: Boolean,
  result: Option[AbstractFile],
  messages: Seq[SnippetCompilerMessage]
):
  def reportMessages()(using CompilerContext) = messages.foreach(_.emit())

enum MessageLevel(val text: String):
  case Info extends MessageLevel("Info")
  case Warning extends MessageLevel("Warning")
  case Error extends MessageLevel("Error")
  case Debug extends MessageLevel("Debug")
