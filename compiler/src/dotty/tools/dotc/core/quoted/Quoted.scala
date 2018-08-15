package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.ast.Trees.GenericApply
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.util.Result

/** Extractors for quotes */
object Quoted {

  /** Extracts the content of a quoted tree.
   *  The result can be the contents of a term ot type quote, which
   *  will return a term or type tree respectively.
   */
  def unapply(tree: tpd.Tree)(implicit ctx: Context): Result[tpd.Tree] = tree match {
    case tree: GenericApply[Type] if tree.symbol.isQuote => Result(tree.args.head)
    case _ => Result.empty
  }
}
