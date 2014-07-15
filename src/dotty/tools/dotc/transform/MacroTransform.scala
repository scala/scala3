package dotty.tools.dotc
package transform

import core._
import typer._
import Phases._
import ast.Trees._
import Contexts._
import Symbols._
import Decorators._

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
abstract class MacroTransform extends Phase {

  import ast.tpd._

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = newTransformer.transform(unit.tpdTree)
  }

  protected def newTransformer(implicit ctx: Context): Transformer

  class Transformer extends TreeMap {

    protected def localCtx(tree: Tree)(implicit ctx: Context) =
      ctx.fresh.setTree(tree).setOwner(tree.symbol)

    /** The current enclosing class
     *  @pre  We must be inside a class
     */
    def currentClass(implicit ctx: Context): ClassSymbol = ctx.owner.enclosingClass.asClass

    def transformStats(trees: List[Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {
      val exprCtx = ctx.withOwner(exprOwner)
      def transformStat(stat: Tree): Tree = stat match {
        case _: Import | _: DefTree => transform(stat)
        case Thicket(stats) => cpy.Thicket(stat, stats mapConserve transformStat)
        case _ => transform(stat)(exprCtx)
      }
      flatten(trees.mapconserve(transformStat(_)))
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      tree match {
        case _: PackageDef | _: MemberDef =>
          super.transform(tree)(localCtx(tree))
        case Template(constr, parents, self, body) =>
          cpy.Template(tree,
            transformSub(constr),
            transform(parents),
            transformSub(self),
            transformStats(body, tree.symbol))
        case _ =>
          super.transform(tree)
      }
    }
  }
}
