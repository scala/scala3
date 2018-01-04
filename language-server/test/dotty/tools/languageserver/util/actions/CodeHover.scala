package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import org.eclipse.lsp4j._

import PositionContext._

class CodeHover(val range: CodeRange, expected: String) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val res = server.hover(fix(marker.toTextDocumentPositionParams)).get()
    assert(res.getRange == null)
    if (expected == "") assert(res.getContents == null, "Expected null contents in " + res)
    else {
      assert(res.getContents.size() == 1, res)
      val content = res.getContents.get(0)
      assert(content.isLeft, "Expected left but was " + content)
      assert(content.getLeft == expected, s"Expected $expected but was ${content.getLeft}")
    }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeHover(${range.show}, $expected"
}
