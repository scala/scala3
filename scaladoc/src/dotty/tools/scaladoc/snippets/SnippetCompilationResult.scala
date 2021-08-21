package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }
import dotty.tools.dotc.util.{ SourcePosition, SrcPos }

case class Position(srcPos: SourcePosition, relativeLine: Int)

case class SnippetCompilerMessage(position: Option[Position], message: String, level: MessageLevel)

case class SnippetCompilationResult(
  wrappedSnippet: WrappedSnippet,
  isSuccessful: Boolean,
  result: Option[AbstractFile],
  messages: Seq[SnippetCompilerMessage]
):
  def reportMessages()(using CompilerContext) = messages.foreach {
    case SnippetCompilerMessage(posOpt, msg, level) =>
      val pos: SrcPos = posOpt.fold(dotty.tools.dotc.util.NoSourcePosition)(_.srcPos)
      level match {
        case MessageLevel.Info => report.log(msg, pos)
        case MessageLevel.Warning => report.warning(msg, pos)
        case MessageLevel.Error => report.error(msg, pos)
        case MessageLevel.Debug => report.log(msg, pos)
      }
  }

enum MessageLevel(val text: String):
  case Info extends MessageLevel("Info")
  case Warning extends MessageLevel("Warning")
  case Error extends MessageLevel("Error")
  case Debug extends MessageLevel("Debug")

