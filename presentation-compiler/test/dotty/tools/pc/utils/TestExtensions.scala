package dotty.tools.pc.utils

import org.eclipse.lsp4j.Position

object TestExtensions:
  extension (pos: Position)
    def getOffset(text: String): Int =
      val lines = text.linesWithSeparators.toList
      lines.take(pos.getLine).foldRight(0)(_.size + _) + pos.getCharacter
