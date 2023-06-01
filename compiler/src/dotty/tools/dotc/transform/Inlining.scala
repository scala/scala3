package dotty.tools.dotc
package transform

import core._
import Flags._
import Contexts._
import Symbols._
import SymUtils._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.staging.StagingLevel
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.StdNames.str
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.Name

import scala.collection.mutable.ListBuffer

/** Inlines all calls to inline methods that are not in an inline method or a quote */
class Inlining extends MacroTransform, SymTransformer {

  import tpd._

  override def phaseName: String = Inlining.name

  override def description: String = Inlining.description

  override def allowsImplicitSearch: Boolean = true

  override def changesMembers: Boolean = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.needsInlining || ctx.compilationUnit.hasMacroAnnotations then
      try super.run
      catch case _: CompilationUnit.SuspendException => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val newUnits = super.runOn(units).filterNot(_.suspended)
    ctx.run.nn.checkSuspendedUnits(newUnits)
    newUnits

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
        new TreeTraverser {
          def traverse(tree: Tree)(using Context): Unit =
            tree match
              case tree: RefTree if !Inlines.inInlineContext && StagingLevel.level == 0 =>
                assert(!tree.symbol.isInlineMethod, tree.show)
              case _ =>
                traverseChildren(tree)
        }.traverse(tree)
      case _ =>
    }

  def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree =
      new InliningTreeMap().transform(tree)
  }

  override def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    if symd.isClass && symd.owner.isInlineTrait && !symd.is(Module) then
      symd.copySymDenotation(name = newInnerClassName(symd.name), initFlags = (symd.flags &~ Final) | Trait)
    else
      symd

  def transformInlineTrait(inlineTrait: TypeDef)(using Context): TypeDef =
    val tpd.TypeDef(_, tmpl: Template) = inlineTrait: @unchecked
    val body1 = tmpl.body.flatMap {
      case innerClass: TypeDef if innerClass.symbol.isClass =>
        val newTrait = makeTraitFromInnerClass(innerClass)
        val newType = makeTypeFromInnerClass(inlineTrait.symbol, innerClass, newTrait.symbol)
        List(newTrait, newType)
      case member: MemberDef =>
        List(member)
      case _ =>
        // Remove non-memberdefs, as they are normally placed into $init()
        Nil
    }
    val tmpl1 = cpy.Template(tmpl)(body = body1)
    cpy.TypeDef(inlineTrait)(rhs = tmpl1)

  private def makeTraitFromInnerClass(innerClass: TypeDef)(using Context): TypeDef =
    val TypeDef(name, tmpl: Template) = innerClass: @unchecked
    val newInnerParents = tmpl.parents.mapConserve(ConcreteParentStripper.apply)
    val tmpl1 = cpy.Template(tmpl)(parents = newInnerParents) // TODO .withType(???)
    cpy.TypeDef(innerClass)(name = newInnerClassName(name), rhs = tmpl1)
  end makeTraitFromInnerClass

  private def makeTypeFromInnerClass(parentSym: Symbol, innerClass: TypeDef, newTraitSym: Symbol)(using Context): TypeDef =
    val upperBound = innerClass.symbol.primaryConstructor.info match {
      case _: MethodType =>
        newTraitSym.typeRef
      case poly: PolyType =>
        HKTypeLambda(poly.paramNames)(tl => poly.paramInfos, tl => newTraitSym.typeRef.appliedTo(tl.paramRefs.head))
    }
    val newTypeSym = newSymbol(
      owner = parentSym,
      name = newTraitSym.name.asTypeName,
      flags = innerClass.symbol.flags & (Private | Protected),
      info = TypeBounds.upper(upperBound),
      privateWithin = innerClass.symbol.privateWithin,
      coord = innerClass.symbol.coord,
      nestingLevel = innerClass.symbol.nestingLevel,
    ).asType
    TypeDef(newTypeSym)
  end makeTypeFromInnerClass

  private class InliningTreeMap extends TreeMapWithImplicits {

    /** List of top level classes added by macro annotation in a package object.
     *  These are added to the PackageDef that owns this particular package object.
     */
    private val newTopClasses = MutableSymbolMap[ListBuffer[Tree]]()

    override def transform(tree: Tree)(using Context): Tree = {
      tree match
        case tree: TypeDef if tree.symbol.isInlineTrait =>
          transformInlineTrait(tree)
        case tree: TypeDef if Inlines.needsInlining(tree) =>
          val tree1 = super.transform(tree).asInstanceOf[TypeDef]
          if tree1.tpe.isError then tree1
          else if tree1.symbol.isInlineTrait then transformInlineTrait(tree1)
          else Inlines.inlineParentInlineTraits(tree1)
        case tree: MemberDef =>
          if tree.symbol.is(Inline) then tree
          else if tree.symbol.is(Param) then super.transform(tree)
          else if
            !tree.symbol.isPrimaryConstructor
            && StagingLevel.level == 0
            && MacroAnnotations.hasMacroAnnotation(tree.symbol)
          then
            val trees = (new MacroAnnotations).expandAnnotations(tree)
            val trees1 = trees.map(super.transform)

            // Find classes added to the top level from a package object
            val (topClasses, trees2) =
              if ctx.owner.isPackageObject then trees1.partition(_.symbol.owner == ctx.owner.owner)
              else (Nil, trees1)
            if topClasses.nonEmpty then
              newTopClasses.getOrElseUpdate(ctx.owner.owner, new ListBuffer) ++= topClasses

            flatTree(trees2)
          else super.transform(tree)
        case _: Typed | _: Block =>
          super.transform(tree)
        case _ if Inlines.needsInlining(tree) =>
          val tree1 = super.transform(tree)
          if tree1.tpe.isError then tree1
          else Inlines.inlineCall(tree1)
        case _: PackageDef =>
          super.transform(tree) match
            case tree1: PackageDef  =>
              newTopClasses.get(tree.symbol.moduleClass) match
                case Some(topClasses) =>
                  newTopClasses.remove(tree.symbol.moduleClass)
                  val newStats = tree1.stats ::: topClasses.result()
                  cpy.PackageDef(tree1)(tree1.pid, newStats)
                case _ => tree1
            case tree1 => tree1
        case _ =>
          if tree.isType then tree
          else super.transform(tree)
    }
  }

  private def newInnerClassName(name: Name): name.ThisName = name ++ str.INLINE_TRAIT_INNER_CLASS_SUFFIX

  private object ConcreteParentStripper extends TreeAccumulator[Tree] {
    def apply(tree: Tree)(using Context): Tree = apply(tree, tree)

    override def apply(x: Tree, tree: Tree)(using Context): Tree = tree match {
      case ident: Ident => ident
      case tpt: TypeTree => tpt
      case _ => foldOver(x, tree)
    }
  }
}

object Inlining:
  val name: String = "inlining"
  val description: String = "inline and execute macros"
