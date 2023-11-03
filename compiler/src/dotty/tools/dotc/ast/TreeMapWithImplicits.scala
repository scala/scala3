package dotty.tools.dotc
package ast

import Trees.*
import core.Contexts.*
import core.ContextOps.enter
import core.Flags.*
import core.Symbols.*
import core.TypeError

/** A TreeMap that maintains the necessary infrastructure to support
 *  contextual implicit searches (type-scope implicits are supported anyway).
 *
 *  This incudes implicits defined in scope as well as imported implicits.
 */
class TreeMapWithImplicits extends tpd.TreeMapWithPreciseStatContexts {
  import tpd.*

  def transformSelf(vd: ValDef)(using Context): ValDef =
    cpy.ValDef(vd)(tpt = transform(vd.tpt))

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
    pattern.foreachSubTree {
      case d: DefTree if d.symbol.isOneOf(GivenOrImplicitVal) => nestedCtx.enter(d.symbol)
      case _ =>
    }
    nestedCtx
  }

  override def transform(tree: Tree)(using Context): Tree = {
    try tree match {
      case Block(stats, expr) =>
        super.transform(tree)(using nestedScopeCtx(stats))
      case tree: DefDef =>
        inContext(localCtx(tree)) {
          cpy.DefDef(tree)(
            tree.name,
            transformParamss(tree.paramss),
            transform(tree.tpt),
            transform(tree.rhs)(using nestedScopeCtx(tree.paramss.flatten)))
        }
      case impl @ Template(constr, _, self, _) =>
        cpy.Template(tree)(
          transformSub(constr),
          transform(impl.parents)(using ctx.superCallContext),
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

