package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.staging.StagingLevel.level

/** This phase removes all non-pickled quotes */
class PruneQuotes extends MiniPhase { thisTransform =>
  import tpd._
  import PruneQuotes._

  override def phaseName: String = PruneQuotes.name

  override def description: String = PruneQuotes.description

  override def transformQuote(tree: Quote)(using Context): Tree =
    if Inlines.inInlineMethod || level > 0 then tree
    else Thicket()
}

object PruneQuotes {
  import tpd._

  val name: String = "pruneQuotes"
  val description: String = "Drop non-pickled copies of the quotes. Only keep the pickled version."
}
