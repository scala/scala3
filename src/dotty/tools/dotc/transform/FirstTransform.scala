package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.transform.TreeTransforms.{AnnotationTransformer, TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import SymDenotations._
import Decorators._
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import scala.collection.mutable
import DenotTransformers._
import typer.Checking
import Names.Name
import NameOps._
import StdNames._


/** The first tree transform
 *   - ensures there are companion objects for all classes except module classes
 *   - eliminates some kinds of trees: Imports, NamedArgs, all TypTrees other than TypeTree
 *   - converts Select/Ident/SelectFromTypeTree nodes that refer to types to TypeTrees.
 *   - inserts `.package` for selections of package object members
 *   - checks the bounds of AppliedTypeTrees
 *   - stubs out native methods
 *   - removes java-defined ASTs
 */
class FirstTransform extends MiniPhaseTransform with IdentityDenotTransformer with AnnotationTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "firstTransform"
  
  override def runsAfter = Set(classOf[typer.InstChecks]) 
    // This phase makes annotations disappear in types, so InstChecks should
    // run before so that it can get at all annotations.

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case Select(qual, _) if tree.symbol.exists =>
      assert(qual.tpe derivesFrom tree.symbol.owner, i"non member selection of ${tree.symbol.showLocated} from ${qual.tpe}")
    case _: TypeTree =>
    case _: Import | _: NamedArg | _: TypTree =>
      assert(false, i"illegal tree: $tree")
    case _ =>
  }

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

    def newCompanion(name: TermName, forClass: Symbol): Thicket = {
      val modul = ctx.newCompleteModuleSymbol(ctx.owner, name, Synthetic, Synthetic,
        defn.ObjectClass.typeRef :: Nil, Scopes.newScope)
      val mc = modul.moduleClass
      if (ctx.owner.isClass) modul.enteredAfter(thisTransformer)
      ctx.synthesizeCompanionMethod(nme.COMPANION_CLASS_METHOD, forClass, mc).enteredAfter(thisTransformer)
      ctx.synthesizeCompanionMethod(nme.COMPANION_MODULE_METHOD, mc, forClass).enteredAfter(thisTransformer)
      ModuleDef(modul, Nil)
    }

    def addMissingCompanions(stats: List[Tree]): List[Tree] = stats map {
      case stat: TypeDef if singleClassDefs contains stat.name =>
        val objName = stat.name.toTermName
        val nameClash = stats.exists {
          case other: MemberDef =>
            other.name == objName && other.symbol.info.isParameterless
          case _ =>
            false
        }
        val uniqueName = if (nameClash) objName.avoidClashName else objName
        Thicket(stat :: newCompanion(uniqueName, stat.symbol).trees)
      case stat => stat
    }

    def skipJava(stats: List[Tree]): List[Tree] = // packages get a JavaDefined flag. Dont skip them
      stats.filter(t => !(t.symbol is(Flags.JavaDefined, Flags.Package)))

    addMissingCompanions(reorder(skipJava(stats)))
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo) = {
    if (ddef.symbol.hasAnnotation(defn.NativeAnnot)) {
      ddef.symbol.resetFlag(Deferred)
      DefDef(ddef.symbol.asTerm,
        _ => ref(defn.Sys_error).withPos(ddef.pos)
          .appliedTo(Literal(Constant("native method stub"))))
    } else ddef
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] =
    ast.Trees.flatten(reorderAndComplete(trees)(ctx.withPhase(thisTransformer.next)))

  private def normalizeType(tree: Tree)(implicit ctx: Context) =
    if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos) else tree

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) = tree.tpe match {
    case tpe: ThisType =>
      /*
       A this reference hide in a self ident, and be subsequently missed
        when deciding on whether outer accessors are needed and computing outer paths.
        We do this normalization directly after Typer, because during typer the
        ident should rest available for hyperlinking.*/
      This(tpe.cls).withPos(tree.pos)
    case _ => normalizeType(tree)
  }



  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) =
    normalizeType {
      val qual = tree.qualifier
      qual.symbol.moduleClass.denot match {
        case pkg: PackageClassDenotation if !tree.symbol.maybeOwner.is(Package) =>
          cpy.Select(tree)(qual select pkg.packageObj.symbol, tree.name)
        case _ =>
          tree
      }
    }

  override def transformSelectFromTypeTree(tree: SelectFromTypeTree)(implicit ctx: Context, info: TransformerInfo) =
    normalizeType(tree)

  override def transformOther(tree: Tree)(implicit ctx: Context, info: TransformerInfo) = tree match {
    case tree: Import => EmptyTree
    case tree: NamedArg => transform(tree.arg)
    case AppliedTypeTree(tycon, args) =>
      val tparams = tycon.tpe.typeSymbol.typeParams
      val bounds = tparams.map(tparam =>
        tparam.info.asSeenFrom(tycon.tpe.normalizedPrefix, tparam.owner.owner).bounds)
      Checking.checkBounds(args, bounds, _.substDealias(tparams, _))
      normalizeType(tree)
    case tree =>
      normalizeType(tree)
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
