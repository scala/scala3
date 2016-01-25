package dotty.tools.dotc.reporting

import java.io.File

import dotty.tools.dotc.callbacks.Position
import dotty.tools.dotc.util.SourcePosition

/**
  * @author Nikolay.Tropin
  */
class CallbackPositionAdapter(sourcePosition: SourcePosition) extends Position {

  override def sourceFile(): File = sourcePosition.source.file.file

  override def sourcePath(): String = sourcePosition.source.file.path

  override def line(): Int = sourcePosition.line

  override def lineContent(): String = sourcePosition.lineContents

  override def column(): Int = sourcePosition.column

  override def offset(): Int = sourcePosition.point
}
