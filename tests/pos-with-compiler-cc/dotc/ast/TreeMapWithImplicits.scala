package dotty.tools.dotc
package ast

import Trees._
import core.Contexts._
import core.ContextOps.enter
import core.Flags._
import core.Symbols._
import core.TypeError

/** A TreeMap that maintains the necessary infrastructure to support
 *  contextual implicit searches (type-scope implicits are supported anyway).
 *
 *  This incudes implicits defined in scope as well as imported implicits.
 */
class TreeMapWithImplicits extends tpd.TreeMapWithPreciseStatContexts {
  import tpd._

  def transformSelf(vd: ValDef)(using Context): ValDef =
    cpy.ValDef(vd)(tpt = transform(vd.tpt))

  private inline def inNestedScopeCtx(defs: List[Tree])(inline op: Context ?-> Tree)(using Context): Tree =
    inMappedContext(ctx =>
        val nestedCtx = ctx.nextFresh.setNewScope
        defs.foreach:
          case d: DefTree if d.symbol.isOneOf(GivenOrImplicitVal) => nestedCtx.enter(d.symbol)
          case _ =>
        nestedCtx
      )(op)

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
        inNestedScopeCtx(stats)(super.transform(tree))
      case tree: DefDef =>
        inLocalCtx(tree):
          cpy.DefDef(tree)(
            tree.name,
            transformParamss(tree.paramss),
            transform(tree.tpt),
            inNestedScopeCtx(tree.paramss.flatten)(transform(tree.rhs)))
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

