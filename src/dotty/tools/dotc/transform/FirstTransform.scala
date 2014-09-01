package dotty.tools.dotc
package transform

import core._
import Names._
import TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import scala.collection.mutable
import DenotTransformers._
import typer.Checking
import Names.Name
import NameOps._


/** The first tree transform
 *   - ensures there are companion objects for all classes except module classes
 *   - eliminates some kinds of trees: Imports, NamedArgs, all TypTrees other than TypeTree
 *   - converts Select/Ident/SelectFromTypeTree nodes that refer to types to TypeTrees.
 *   - checks the bounds of AppliedTypeTrees
 *   - stubs out native methods
 */
class FirstTransform extends MiniPhaseTransform with IdentityDenotTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "companions"

  /** Reorder statements so that module classes always come after their companion classes, add missing companion classes */
  private def reorderAndComplete(stats: List[Tree])(implicit ctx: Context): List[Tree] = {
    val moduleClassDefs, singleClassDefs = mutable.Map[Name, Tree]()

    def reorder(stats: List[Tree]): List[Tree] = stats match {
      case (stat: TypeDef) :: stats1 if stat.symbol.isClass =>
        if (stat.symbol is Flags.Module) {
          moduleClassDefs += (stat.name -> stat)
          singleClassDefs -= stat.name.stripModuleClassSuffix
          val stats1r = reorder(stats1)
          if (moduleClassDefs contains stat.name) stat :: stats1r else stats1r
        } else {
          def stats1r = reorder(stats1)
          val normalized = moduleClassDefs remove stat.name.moduleClassName match {
            case Some(mcdef) =>
              mcdef :: stats1r
            case None =>
              singleClassDefs += (stat.name -> stat)
              stats1r
          }
          stat :: normalized
        }
      case stat :: stats1 => stat :: reorder(stats1)
      case Nil => Nil
    }

    def newCompanion(name: TermName): Thicket = {
      val modul = ctx.newCompleteModuleSymbol(ctx.owner, name, Synthetic, Synthetic,
        defn.ObjectClass.typeRef :: Nil, Scopes.newScope)
      if (ctx.owner.isClass) modul.enteredAfter(thisTransformer)
      ModuleDef(modul, Nil)
    }

    def addMissingCompanions(stats: List[Tree]): List[Tree] = stats map {
      case stat: TypeDef if singleClassDefs contains stat.name =>
        Thicket(stat :: newCompanion(stat.name.toTermName).trees)
      case stat => stat
    }
    addMissingCompanions(reorder(stats))
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo) =
    if (ddef.symbol.hasAnnotation(defn.NativeAnnot)) {
      ddef.symbol.resetFlag(Deferred)
      DefDef(ddef.symbol.asTerm,
        _ => ref(defn.Sys_error).withPos(ddef.pos)
          .appliedTo(Literal(Constant("native method stub"))))
    } else ddef

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] =
    ast.Trees.flatten(reorderAndComplete(trees)(ctx.withPhase(thisTransformer.next)))

  private def normalizeType(tree: Tree)(implicit ctx: Context) =
    if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos) else tree

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = tree.tpe match {
    case tpe: ThisType => This(tpe.cls).withPos(tree.pos)
    case _ => normalizeType(tree)
  }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) =
    normalizeType(tree)

  override def transformSelectFromTypeTree(tree: SelectFromTypeTree)(implicit ctx: Context, info: TransformerInfo) =
    normalizeType(tree)

  override def transformOther(tree: Tree)(implicit ctx: Context, info: TransformerInfo) = tree match {
    case tree: Import => EmptyTree
    case tree: NamedArg => tree.arg
    case AppliedTypeTree(tycon, args) =>
      val tparams = tycon.tpe.typeSymbol.typeParams
      Checking.checkBounds(
        args, tparams.map(_.info.bounds), (tp, argTypes) => tp.substDealias(tparams, argTypes))
      normalizeType(tree)
    case tree =>
      normalizeType(tree)
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
