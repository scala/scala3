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
 *   - eliminates some kinds of trees: Imports, NamedArgs
 *   - stubs out native methods
 */
class FirstTransform extends MiniPhaseTransform with IdentityDenotTransformer with AnnotationTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "firstTransform"

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
        defn.ObjectType :: Nil, Scopes.newScope)
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

    addMissingCompanions(reorder(stats))
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context, info: TransformerInfo) = {
    if (ddef.symbol.hasAnnotation(defn.NativeAnnot)) {
      ddef.symbol.resetFlag(Deferred)
      DefDef(ddef.symbol.asTerm,
        _ => ref(defn.Sys_errorR).withPos(ddef.pos)
          .appliedTo(Literal(Constant("native method stub"))))
    } else ddef
  }

  override def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] =
    ast.Trees.flatten(reorderAndComplete(trees)(ctx.withPhase(thisTransformer.next)))

  override def transformOther(tree: Tree)(implicit ctx: Context, info: TransformerInfo) = tree match {
    case tree: Import => EmptyTree
    case tree: NamedArg => transform(tree.arg)
    case tree => tree
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
