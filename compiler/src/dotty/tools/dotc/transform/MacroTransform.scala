package dotty.tools.dotc
package transform

import core._
import Phases._
import ast.Trees._
import Contexts._
import Symbols._
import Flags.PackageVal
import Decorators._

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
abstract class MacroTransform extends Phase {

  import ast.tpd._

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = newTransformer.transform(unit.tpdTree)(ctx.withPhase(transformPhase))
  }

  protected def newTransformer(implicit ctx: Context): Transformer

  /** The phase in which the transformation should be run.
   *  By default this is the phase given by the this macro transformer,
   *  but it could be overridden to be the phase following that one.
   */
  protected def transformPhase(implicit ctx: Context): Phase = this

  class Transformer extends TreeMap(cpy = cpyBetweenPhases) {

    protected def localCtx(tree: Tree)(implicit ctx: Context): FreshContext = {
      val sym = tree.symbol
      val owner = if (sym is PackageVal) sym.moduleClass else sym
      ctx.fresh.setTree(tree).setOwner(owner)
    }

    def transformStats(trees: List[Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {
      def transformStat(stat: Tree): Tree = stat match {
        case _: Import | _: DefTree => transform(stat)
        case _ => transform(stat)(ctx.exprContext(stat, exprOwner))
      }
      flatten(trees.mapconserve(transformStat(_)))
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      try
        tree match {
          case EmptyValDef =>
            tree
          case _: PackageDef | _: MemberDef =>
            super.transform(tree)(localCtx(tree))
          case impl @ Template(constr, parents, self, _) =>
            cpy.Template(tree)(
              transformSub(constr),
              transform(parents)(ctx.superCallContext),
              Nil,
              transformSelf(self),
              transformStats(impl.body, tree.symbol))
          case _ =>
            super.transform(tree)
        }
      catch {
        case ex: TypeError =>
          ctx.error(ex.toMessage, tree.sourcePos, sticky = true)
          tree
      }

    def transformSelf(vd: ValDef)(implicit ctx: Context): ValDef =
      cpy.ValDef(vd)(tpt = transform(vd.tpt))
  }
}
