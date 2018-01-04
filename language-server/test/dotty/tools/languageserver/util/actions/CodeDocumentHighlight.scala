package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

class CodeDocumentHighlight(val range: CodeRange, highs: Seq[(CodeRange, String)]) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val (refs, kinds) = highs.unzip
    val res = server.documentHighlight(fix(marker.toTextDocumentPositionParams)).get()
    assert(res.size() == refs.size, res)
    assert(refs.size == kinds.length, res)
    res.asScala.zip(refs).zip(kinds).foreach { case ((dhl, ref), kind) =>
      assert(dhl.getKind == DocumentHighlightKind.valueOf(kind), res)
      assert(dhl.getRange == ref.toRange, res)
    }
  }

  override def show: PositionContext.PosCtx[String] = {
    val (refs, kinds) = highs.unzip
    s"CodeDocumentHighlight(${range.show}, ${refs.map(_.show)}, $kinds)"
  }
}
