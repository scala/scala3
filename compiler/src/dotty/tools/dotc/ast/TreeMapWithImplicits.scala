package dotty.tools.dotc.ast

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.TypeError

import scala.annotation.tailrec

/** A TreeMap that maintains the necessary infrastructure to support
 *  contxtual implicit searches (type-scope implicits are supported anyway).
 *
 *  This incudes impicits defined in scope as well as imported implicits.
 */
class TreeMapWithImplicits extends tpd.TreeMap {
  import tpd._

  def transformSelf(vd: ValDef)(implicit ctx: Context): ValDef =
    cpy.ValDef(vd)(tpt = transform(vd.tpt))

  /** Transform statements, while maintaining import contexts and expression contexts
   *  in the same way as Typer does. The code addresses additional concerns:
   *   - be tail-recursive where possible
   *   - don't re-allocate trees where nothing has changed
   */
  def transformStats(stats: List[Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {

    @tailrec def traverse(curStats: List[Tree])(implicit ctx: Context): List[Tree] = {

      def recur(stats: List[Tree], changed: Tree, rest: List[Tree])(implicit ctx: Context): List[Tree] = {
        if (stats eq curStats) {
          val rest1 = transformStats(rest, exprOwner)
          changed match {
            case Thicket(trees) => trees ::: rest1
            case tree => tree :: rest1
          }
        }
        else stats.head :: recur(stats.tail, changed, rest)
      }

      curStats match {
        case stat :: rest =>
          val statCtx = stat match {
            case stat: DefTree => ctx
            case _ => ctx.exprContext(stat, exprOwner)
          }
          val restCtx = stat match {
            case stat: Import => ctx.importContext(stat, stat.symbol)
            case _ => ctx
          }
          val stat1 = transform(stat)(statCtx)
          if (stat1 ne stat) recur(stats, stat1, rest)(restCtx)
          else traverse(rest)(restCtx)
        case nil =>
          stats
      }
    }
    traverse(stats)
  }

  private def nestedScopeCtx(defs: List[Tree])(implicit ctx: Context): Context = {
    val nestedCtx = ctx.fresh.setNewScope
    defs foreach {
      case d: DefTree => nestedCtx.enter(d.symbol)
      case _ =>
    }
    nestedCtx
  }

  override def transform(tree: Tree)(implicit ctx: Context): Tree = {
    def localCtx =
      if (tree.hasType && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
    try tree match {
      case tree: Block =>
        super.transform(tree)(nestedScopeCtx(tree.stats))
      case tree: DefDef =>
        implicit val ctx = localCtx
        cpy.DefDef(tree)(
          tree.name,
          transformSub(tree.tparams),
          tree.vparamss mapConserve (transformSub(_)),
          transform(tree.tpt),
          transform(tree.rhs)(nestedScopeCtx(tree.vparamss.flatten)))
      case EmptyValDef =>
        tree
      case _: PackageDef | _: MemberDef =>
        super.transform(tree)(localCtx)
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
        ctx.error(ex.toMessage, tree.sourcePos)
        tree
    }
  }

}
