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

  def normalizeUnusedExpr(tree: Tree, msg: String)(implicit ctx: Context): Tree = {
    if (!isPureExpr(tree))
      ctx.warning(msg + "This expression will not be evaluated.", tree.pos)
    defaultValue(tree.tpe)
  }

  def normalizeUnusedRhs(tree: Tree, sym: Symbol)(implicit ctx: Context) = {
    if (sym.is(Unused) && tree.tpe.exists) normalizeUnusedExpr(tree, "Expression is on the RHS of an `unused` " + sym.showKind + ". ")
    else tree
  }

}
