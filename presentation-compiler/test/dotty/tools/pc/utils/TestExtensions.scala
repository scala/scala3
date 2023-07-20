package dotty.tools.pc.utils

import org.eclipse.lsp4j.Position

object TestExtensions:
  extension (pos: Position)
    def getOffset(text: String): Int =
      val lines = text.linesWithSeparators.toList
      lines.take(pos.getLine).foldRight(0)(_.size + _) + pos.getCharacter

  extension (range: dotty.tools.dotc.semanticdb.Range)
    def getOffsets(text: String): (Int, Int) =
      val lines = text.linesWithSeparators.toList
      val startOffset = lines
        .take(range.startLine)
        .foldRight(0)(_.size + _) + range.startCharacter
      val endOffset =
        lines.take(range.endLine).foldRight(0)(_.size + _) + range.endCharacter
      (startOffset, endOffset)
