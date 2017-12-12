package dotty.tools.dotc.typer

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._

/** Util methods for transformation of `unused` expressions
 *
 *  @author Nicolas Stucki
 */
object UnusedUtil {

  /** Transforms the tree into a its default tree.
   *  Performed to shrink the tree that is known to be erased later.
   */
  def normalizeUnusedExpr(tree: Tree, msg: String)(implicit ctx: Context): Tree = {
    if (!isPureExpr(tree))
      ctx.warning(msg + "This expression will not be evaluated.", tree.pos)
    defaultValue(tree.tpe)
  }

  /** Transforms the rhs tree into a its default tree if it is in an `unused` val/def.
   *  Performed to shrink the tree that is known to be erased later.
   */
  def normalizeUnusedRhs(rhs: Tree, sym: Symbol)(implicit ctx: Context) = {
    if (sym.is(Unused) && rhs.tpe.exists) normalizeUnusedExpr(rhs, "Expression is on the RHS of an `unused` " + sym.showKind + ". ")
    else rhs
  }

}
