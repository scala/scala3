package scala.meta.internal.pc

import scala.meta.internal.mtags.MtagsEnrichments.*
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourcePosition
import org.eclipse.lsp4j.DocumentHighlight
import org.eclipse.lsp4j.DocumentHighlightKind

final class PcDocumentHighlightProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
) extends PcCollector[DocumentHighlight](driver, params):

  def collect(
      parent: Option[Tree]
  )(
      tree: Tree,
      toAdjust: SourcePosition,
      sym: Option[Symbol],
  ): DocumentHighlight =
    val (pos, _) = adjust(toAdjust)
    tree match
      case _: NamedDefTree =>
        DocumentHighlight(pos.toLsp, DocumentHighlightKind.Write)
      case _ => DocumentHighlight(pos.toLsp, DocumentHighlightKind.Read)

  def highlights: List[DocumentHighlight] =
    result().distinctBy(_.getRange())
end PcDocumentHighlightProvider
