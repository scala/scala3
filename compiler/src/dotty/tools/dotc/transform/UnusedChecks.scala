package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

// TODO
/** THis phase ...
 */
class UnusedChecks extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "unusedChecks"

  /* Tree transform */

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!ctx.owner.is(Unused) && !tree.symbol.is(Unused) && !tree.fun.tpe.widen.isUnusedMethod) {
      for (arg <- tree.args) {
        if (arg.symbol.is(Unused))
          ctx.error(s"Cannot use `unused` value in a context that is not `unused`", arg.pos)
      }
    }
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = checkedValOrDefDef(tree)
  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = checkedValOrDefDef(tree)


  /* private methods */

  private def checkedValOrDefDef(tree: ValOrDefDef)(implicit ctx: Context): Tree = {
    if (!tree.symbol.is(Unused) && !ctx.owner.is(Unused))
      check(tree.rhs)
    tree
  }

  private def check(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case Block(_, expr) => check(expr)
    case _ if tree.symbol.is(Unused) =>
      ctx.error("Cannot return `unused` value in a def without `unused`", tree.pos)
    case _ =>
  }
}
