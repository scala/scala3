package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.SymUtils._

/** Extractors for splices */
object Spliced {

  /** Extracts the content of a spliced tree.
   *  The result can be the contents of a term or type splice, which
   *  will return a term or type tree respectively.
   */
  def unapply(tree: tpd.Select)(implicit ctx: Context): Option[tpd.Tree] =
    if (tree.symbol.isSplice) Some(tree.qualifier) else None

}
