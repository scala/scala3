package dotty.tools.dotc
package transform

import core.*
import Phases.*
import ast.Trees.*
import Contexts.*

/** A base class for transforms.
 *  A transform contains a compiler phase which applies a tree transformer.
 */
abstract class MacroTransform extends Phase {

  import ast.tpd.*

  override def isRunnable(using Context) = super.isRunnable && !ctx.usedBestEffortTasty

  protected def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = atPhase(transformPhase)(newTransformer.transform(unit.tpdTree))
  }

  protected def newTransformer(using Context): Transformer

  /** The phase in which the transformation should be run.
   *  By default this is the phase given by the this macro transformer,
   *  but it could be overridden to be the phase following that one.
   */
  protected def transformPhase(using Context): Phase = this

  class Transformer extends TreeMapWithPreciseStatContexts(cpy = cpyBetweenPhases):

    protected def localCtx(tree: Tree)(using Context): FreshContext =
      ctx.fresh.setTree(tree).setOwner(localOwner(tree))

    override def transform(tree: Tree)(using Context): Tree =
      // iter-2: syntactic leaf-tree shortcut. Ident/Literal/This/TypeTree are
      // childless leaves that fall through this match to `super.transform`
      // (the base TreeMap.transform), whose body for these shapes is just
      // `tree`. When transformCtx would return ctx unchanged and skipTransform
      // is false (its default here), skip the try/match + super dispatch.
      // MacroTransform.Transformer is the largest caller of the transform spine.
      if ((tree.isInstanceOf[Ident @unchecked]
            || tree.isInstanceOf[Literal @unchecked]
            || tree.isInstanceOf[This @unchecked]
            || tree.isInstanceOf[TypeTree @unchecked])
          && ((tree.source `eq` ctx.source) || !tree.source.exists)
          && !skipTransform(tree))
        tree
      else
      try
        tree match {
          case EmptyValDef =>
            tree
          case _: PackageDef | _: MemberDef =>
            super.transform(tree)(using localCtx(tree))
          case impl @ Template(constr, _, self, _) =>
            cpy.Template(tree)(
              transformSub(constr),
              transform(impl.parents)(using ctx.superCallContext),
              Nil,
              transformSelf(self),
              transformStats(impl.body, tree.symbol))
          case _ =>
            super.transform(tree)
        }
      catch {
        case ex: TypeError =>
          report.error(ex, tree.srcPos)
          tree
      }

    def transformSelf(vd: ValDef)(using Context): ValDef =
      cpy.ValDef(vd)(tpt = transform(vd.tpt))
  end Transformer
}
