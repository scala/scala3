package dotty.tools.dotc
package ast

import Trees._
import core.Contexts._
import core.ContextOps.enter
import core.Flags._
import core.Symbols._
import core.TypeError

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
   *   - be tail-recursive where possible
   *   - don't re-allocate trees where nothing has changed
   */
  override def transformStats(stats: List[Tree], exprOwner: Symbol)(using Context): List[Tree] = {

    @tailrec def traverse(curStats: List[Tree])(using Context): List[Tree] = {

      def recur(stats: List[Tree], changed: Tree, rest: List[Tree])(using Context): List[Tree] =
        if (stats eq curStats) {
          val rest1 = transformStats(rest, exprOwner)
          changed match {
            case Thicket(trees) => trees ::: rest1
            case tree => tree :: rest1
          }
        }
        else stats.head :: recur(stats.tail, changed, rest)

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
          val stat1 = transform(stat)(using statCtx)
          if (stat1 ne stat) recur(stats, stat1, rest)(using restCtx)
          else traverse(rest)(using restCtx)
        case nil =>
          stats
      }
    }
    traverse(stats)
  }

  private def nestedScopeCtx(defs: List[Tree])(using Context): Context = {
    val nestedCtx = ctx.fresh.setNewScope
    defs foreach {
      case d: DefTree if d.symbol.isOneOf(GivenOrImplicitVal) => nestedCtx.enter(d.symbol)
      case _ =>
    }
    nestedCtx
  }

  private def patternScopeCtx(pattern: Tree)(using Context): Context = {
    val nestedCtx = ctx.fresh.setNewScope
    new TreeTraverser {
      def traverse(tree: Tree)(using Context): Unit = {
        tree match {
          case d: DefTree if d.symbol.isOneOf(GivenOrImplicitVal) =>
            nestedCtx.enter(d.symbol)
          case _ =>
        }
        traverseChildren(tree)
      }
    }.traverse(pattern)
    nestedCtx
  }

  override def transform(tree: Tree)(using Context): Tree = {
    try tree match {
      case Block(stats, expr) =>
        inContext(nestedScopeCtx(stats)) {
          if stats.exists(_.isInstanceOf[Import]) then
            // need to transform stats and expr together to account for import visibility
            val stats1 = transformStats(stats :+ expr, ctx.owner)
            cpy.Block(tree)(stats1.init, stats1.last)
          else super.transform(tree)
        }
      case tree: DefDef =>
        inContext(localCtx(tree)) {
          cpy.DefDef(tree)(
            tree.name,
            transformParamss(tree.paramss),
            transform(tree.tpt),
            transform(tree.rhs)(using nestedScopeCtx(tree.paramss.flatten)))
        }
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

