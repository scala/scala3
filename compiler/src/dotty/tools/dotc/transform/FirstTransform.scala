package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.NeedsCompanions
import dotty.tools.dotc.transform.TreeTransforms._
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
import NameOps._
import NameKinds.{AvoidClashName, OuterSelectName}
import StdNames._


/** The first tree transform
 *   - ensures there are companion objects for all classes except module classes
 *   - eliminates some kinds of trees: Imports, NamedArgs
 *   - stubs out native methods
 *   - eliminates self tree in Template and self symbol in ClassInfo
 *   - collapses all type trees to trees of class TypeTree
 *   - converts idempotent expressions with constant types
 *   - drops branches of ifs using the rules
 *          if (true) A else B  --> A
 *          if (false) A else B --> B
 */
class FirstTransform extends MiniPhaseTransform with InfoTransformer with AnnotationTransformer { thisTransformer =>
  import ast.tpd._

  override def phaseName = "firstTransform"

  private var addCompanionPhases: List[NeedsCompanions] = _

  override def changesMembers = true // the phase adds companion objects

  def needsCompanion(cls: ClassSymbol)(implicit ctx: Context) =
    addCompanionPhases.exists(_.isCompanionNeeded(cls))

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    addCompanionPhases = ctx.phasePlan.flatMap(_ collect { case p: NeedsCompanions => p })
    this
  }

  /** eliminate self symbol in ClassInfo */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp @ ClassInfo(_, _, _, _, self: Symbol) =>
      tp.derivedClassInfo(selfInfo = self.info)
    case _ =>
      tp
  }

  /*
      tp match {
        //create companions for value classes that are not from currently compiled source file
        case tp@ClassInfo(_, cls, _, decls, _)
          if (ValueClasses.isDerivedValueClass(cls)) &&
            !sym.isDefinedInCurrentRun && sym.scalacLinkedClass == NoSymbol =>
          val newDecls = decls.cloneScope
          val (modul, mcMethod, symMethod) = newCompanion(sym.name.toTermName, sym)
          modul.entered
          mcMethod.entered
          newDecls.enter(symMethod)
          tp.derivedClassInfo(decls = newDecls)
        case _ => tp
      }
  }
  */

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case Select(qual, name) if !name.is(OuterSelectName) && tree.symbol.exists =>
        assert(qual.tpe derivesFrom tree.symbol.owner, i"non member selection of ${tree.symbol.showLocated} from ${qual.tpe} in $tree")
      case _: TypeTree =>
      case _: Import | _: NamedArg | _: TypTree =>
        assert(false, i"illegal tree: $tree")
      case _ =>
    }
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

    def registerCompanion(name: TermName, forClass: Symbol): TermSymbol = {
      val (modul, mcCompanion, classCompanion) = newCompanion(name, forClass)
      if (ctx.owner.isClass) modul.enteredAfter(thisTransformer)
      mcCompanion.enteredAfter(thisTransformer)
      classCompanion.enteredAfter(thisTransformer)
      modul
    }

    def addMissingCompanions(stats: List[Tree]): List[Tree] = stats map {
      case stat: TypeDef if (singleClassDefs contains stat.name) && needsCompanion(stat.symbol.asClass) =>
        val objName = stat.name.toTermName
        val nameClash = stats.exists {
          case other: MemberDef =>
            other.name == objName && other.symbol.info.isParameterless
          case _ =>
            false
        }
        val uniqueName = if (nameClash) AvoidClashName(objName) else objName
        Thicket(stat :: ModuleDef(registerCompanion(uniqueName, stat.symbol), Nil).trees)
      case stat => stat
    }

    addMissingCompanions(reorder(stats))
  }

  private def newCompanion(name: TermName, forClass: Symbol)(implicit ctx: Context) = {
    val modul = ctx.newCompleteModuleSymbol(forClass.owner, name, Synthetic, Synthetic,
      defn.ObjectType :: Nil, Scopes.newScope, assocFile = forClass.asClass.assocFile)
    val mc = modul.moduleClass

    val mcComp = ctx.synthesizeCompanionMethod(nme.COMPANION_CLASS_METHOD, forClass, mc)
    val classComp = ctx.synthesizeCompanionMethod(nme.COMPANION_MODULE_METHOD, mc, forClass)
    (modul, mcComp, classComp)
  }

  /** elimiate self in Template */
  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    cpy.Template(impl)(self = EmptyValDef)
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
    case tree => if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos) else tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo) =
    if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos)
    else constToLiteral(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo) =
    if (tree.isType) TypeTree(tree.tpe).withPos(tree.pos)
    else constToLiteral(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo) =
    constToLiteral(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) =
    constToLiteral(tree)

  override def transformTyped(tree: Typed)(implicit ctx: Context, info: TransformerInfo) =
    constToLiteral(tree)

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo) =
    constToLiteral(tree)

  override def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo) =
    tree.cond match {
      case Literal(Constant(c: Boolean)) => if (c) tree.thenp else tree.elsep
      case _ => tree
    }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
