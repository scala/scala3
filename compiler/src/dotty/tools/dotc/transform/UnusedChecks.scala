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

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): tree.type = {
    if (tree.symbol.is(UnusedCommon))
      ctx.error(tree.symbol.showKind + " cannot be unused", tree.pos)
    tree
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): tree.type = {
    if (!isUnusedContext && !tree.symbol.is(Unused) && !tree.fun.tpe.widen.isUnusedMethod)
      tree.args.foreach(arg => checked(arg)(arg, "Cannot use `unused` value in a context that is not `unused`"))
    tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checkedValOrDefDefRHS(tree)

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): tree.type = {
    if (tree.symbol.is(Unused) && tree.symbol.is(MutableOrLazy))
      ctx.error(tree.symbol.showKind + " cannot be unused", tree.pos)
    checkedValOrDefDefRHS(tree)
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checked(tree)(tree.qualifier, "Cannot use `unused` value in a context that is not `unused`")

  override def transformMatch(tree: Match)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checked(tree)(tree.selector, "Cannot match on `unused` value in a context that is not `unused`")

  override def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checked(tree)(tree.cond, "Cannot use `unused` condition in a context that is not `unused`")

  override def transformReturn(tree: Return)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checked(tree)(tree.expr, "Cannot return `unused` condition in a context that is not `unused`")

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): tree.type =
    checked(tree)(tree.rhs, "Cannot assign `unused` condition in a context that is not `unused`")


  /* private methods */

  private def checkedValOrDefDefRHS(tree: ValOrDefDef)(implicit ctx: Context): tree.type =
    if (tree.symbol.is(Unused)) tree
    else checked(tree)(tree.rhs, "Cannot return `unused` value in a def without `unused`")

  private def checked(tree0: Tree)(tree1: Tree, msg: String)(implicit ctx: Context): tree0.type = {
    def rec(t: Tree): Unit = t match {
      case Block(_, expr) => rec(expr)
      case If(_, thenp, elsep) =>
        rec(thenp)
        rec(elsep)
      case Match(_, cases) =>
        cases.foreach(c => rec(c.body))
      case Try(expr, cases, _) =>
        rec(expr)
        cases.foreach(c => rec(c.body))
      case _ if t.symbol.is(Unused) =>
        ctx.error(msg, t.pos)
      case _ =>
    }
    if (!isUnusedContext)
      rec(tree1)
    tree0
  }

  def isUnusedContext(implicit ctx: Context): Boolean =
    ctx.owner.ownersIterator.exists(_.is(Unused)) // TODO make context mode?
}
