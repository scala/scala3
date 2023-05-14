package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.embedded.{CodeInRange, CodeMarker}
import dotty.tools.languageserver.util.server.TestFile

import org.eclipse.lsp4j.{Location, Range, SymbolKind}

import PositionContext._

/**
 * A range of positions between two markers.
 *
 * @param start The start marker.
 * @param end   The end marker.
 */
case class CodeRange(start: CodeMarker, end: CodeMarker) {
  private[this] var checked = false
  def check(): PosCtx[Unit] = {
    if (!checked) {
      assert(start.file == end.file, s"$start and $end where not in the same file")
      assert(start.line <= end.line, s"Expected $end to be after $start")
      assert(start.line != end.line || start.character <= end.character, s"Expected $end to be at or after $start")
      checked = true
    }
  }

  def file: PosCtx[TestFile] = {
    check()
    start.file
  }

  def withCode(text: String): CodeInRange = CodeInRange(text, this)

  def symInfo(name: String, kind: SymbolKind, container: String = null): SymInfo =
    new SymInfo(name, kind, this, container)

  def toRange: PosCtx[Range] = {
    check()
    new Range(start.toPosition, end.toPosition)
  }

  def toLocation: PosCtx[Location] = {
    check()
    new Location(file.uri, toRange)
  }

  def show: PosCtx[String] =
    s"[start=${start.show}, end=${end.show}]"
}
