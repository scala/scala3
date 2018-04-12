package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import dotty.tools.languageserver.util.embedded.CodeMarker

/** An action that executes on a range. */
trait ActionOnRange extends Action {

  /** The range on which the action is performed. */
  def range: CodeRange

  /** The action to perform for every point of the range. */
  def onMarker(marker: CodeMarker): Exec[Unit]

  override def execute(): Exec[Unit] = {
    val posCtx = implicitly[PositionContext]
    range.check()
    val file = range.file
    val start = range.start
    val startLine = start.line
    val startCharacter = start.character
    val endLine = range.end.line
    val endCharacter = range.end.character
    assert(startLine <= endLine, "Start of range should be before end " + range.show)
    assert(startLine != endLine || startCharacter < endCharacter, "Start of range should be before end " + range.show)
    assert(startLine == endLine, "multiline ranges not supported") // TODO implement multiline
    (startCharacter until endCharacter).foreach { char =>
      val marker = new CodeMarker(start.name + "-with-offset=" + (char - startCharacter))
      implicit def posCtx2: PositionContext = posCtx.withPos(marker, (file, startLine, char))
      onMarker(marker)
    }
  }
}
