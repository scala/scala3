package dotty.tools.dotc
package ast

import Trees._
import core.Contexts._
import core.ContextOps.enter
import core.Flags._
import core.Symbols._
import core.TypeError
import util.Lst
import util.Lst.{NIL, +:, toLst}


import scala.annotation.tailrec

/** A TreeMap that maintains the necessary infrastructure to support
 *  contextual implicit searches (type-scope implicits are supported anyway).
 *
 *  This incudes implicits defined in scope as well as imported implicits.
 */
class TreeMapWithImplicits extends tpd.TreeMap {
  import tpd._

  def transformSelf(vd: ValDef)(using Context): ValDef =
    cpy.ValDef(vd)(tpt = transform(vd.tpt))

  /** Transform statements, while maintaining import contexts and expression contexts
   *  in the same way as Typer does. The code addresses additional concerns:
   *   - dissolve thickets
   *   - don't re-allocate trees where nothing has changed
   */
  def transformStats(stats: Lst[Tree], exprOwner: Symbol)(using Context): Lst[Tree] =
    var buf: Lst.Buffer[Tree] = null
    var curCtx: Context = ctx
    for n <- 0 until stats.length do
      val stat = stats(n)
      val statCtx = stat match
        case stat: DefTree => curCtx
        case _ => curCtx.exprContext(stat, exprOwner)
      val stat1 = transform(stat)(using statCtx)
      if buf == null && (stat1 ne stat) then
        buf = Lst.Buffer()
        buf.appendSlice(stats, 0, n)
      if buf != null then
        stat1 match
          case Thicket(elems) => buf ++= elems
          case _ => buf += stat1
      curCtx = stat match
        case stat: Import => curCtx.importContext(stat, stat.symbol)
        case _ => curCtx
    if buf == null then stats else buf.toLst

  private def nestedScopeCtx(defs: Lst[Tree])(using Context): Context = {
    val nestedCtx = ctx.fresh.setNewScope
    defs foreach {
      case d: DefTree if d.symbol.isOneOf(GivenOrImplicit) => nestedCtx.enter(d.symbol)
      case _ =>
    }
    nestedCtx
  }

  private def patternScopeCtx(pattern: Tree)(using Context): Context = {
    val nestedCtx = ctx.fresh.setNewScope
    new TreeTraverser {
      def traverse(tree: Tree)(using Context): Unit = {
        tree match {
          case d: DefTree if d.symbol.isOneOf(GivenOrImplicit) =>
            nestedCtx.enter(d.symbol)
          case _ =>
        }
        traverseChildren(tree)
      }
    }.traverse(pattern)
    nestedCtx
  }

  override def transform(tree: Tree)(using Context): Tree = {
    def localCtx =
      if (tree.hasType && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
    try tree match {
      case tree: Block =>
        super.transform(tree)(using nestedScopeCtx(tree.stats))
      case tree: DefDef =>
        inContext(localCtx) {
          cpy.DefDef(tree)(
            tree.name,
            transformSub(tree.tparams),
            tree.vparamss mapConserve (transformSub(_)),
            transform(tree.tpt),
            transform(tree.rhs)(using nestedScopeCtx(tree.vparamss.flatten.toLst)))
        }
      case EmptyValDef =>
        tree
      case _: PackageDef | _: MemberDef =>
        super.transform(tree)(using localCtx)
      case impl @ Template(constr, parents, self, _) =>
        cpy.Template(tree)(
          transformSub(constr),
          transform(parents)(using ctx.superCallContext),
          Nil,
          transformSelf(self),
          transformStats(impl.body, tree.symbol))
      case tree: CaseDef =>
        val patCtx = patternScopeCtx(tree.pat)(using ctx)
        cpy.CaseDef(tree)(
          transform(tree.pat),
          transform(tree.guard)(using patCtx),
          transform(tree.body)(using patCtx)
        )
      case _ =>
        super.transform(tree)
    }
    catch {
      case ex: TypeError =>
        report.error(ex, tree.srcPos)
        tree
    }
  }
}

