package dotty.tools.dotc
package transform

import core._
import Flags._
import Contexts._
import Symbols._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.ast.TreeMapWithImplicits
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.staging.StagingLevel
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.StdNames.{str, nme}
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Names.{Name, TermName}

import scala.collection.mutable.ListBuffer

class SpecializeInlineTraits extends MacroTransform, SymTransformer {

  import tpd._

  override def phaseName: String = SpecializeInlineTraits.name

  override def description: String = SpecializeInlineTraits.description

  override def changesMembers: Boolean = true

  override def changesParents: Boolean = true


  override def run(using Context): Unit =
    try super.run
    catch case _: CompilationUnit.SuspendException => ()

  override def newTransformer(using Context): Transformer = new Transformer {
    override def transform(tree: Tree)(using Context): Tree = tree match {
      case tree: TypeDef if tree.symbol.isInlineTrait =>
        val tree1 = Inlines.transformInlineTrait(tree)
        val tree2 = if Inlines.needsInlining(tree1) then Inlines.inlineParentInlineTraits(tree1) else tree1
        super.transform(tree2) // We may need to inline inline traits into the bodies of methods defined inside inline traits.
      case tree: TypeDef if Inlines.needsInlining(tree) =>
        if tree.symbol.isAllOf(Trait, butNot = Inline) then
          val problemParents = tree.symbol.info.parents.filter(
            p => p.classSymbol.isInlineTrait 
                 && p.classSymbol.primaryConstructor.paramSymss.exists(paramList => paramList.nonEmpty && paramList.head.isTerm)
          )
          problemParents.foreach( p =>
            report.error(s"Only parameterless inline traits may be extended by ordinary traits. Make ${tree.symbol} inline or remove inline ${p.typeSymbol}'s parameter list.", tree.srcPos)
          )
        val tree1 =
          if tree.symbol.isInlineTrait then 
            Inlines.inlineParentInlineTraits(Inlines.transformInlineTrait(tree))
          else Inlines.inlineParentInlineTraits(tree)
        super.transform(tree1)

      case _ => super.transform(tree)
    }
  }

  override def transformSym(symd: SymDenotation)(using Context): SymDenotation =
    if symd.isClass && symd.owner.isInlineTrait && !symd.is(Module) then
      symd.copySymDenotation(name = SpecializeInlineTraits.newInnerClassName(symd.name), initFlags = (symd.flags &~ Final) | Trait)
    else
      symd

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      // TODO check that things are inlined properly
      case _ =>
    }

  /* private def makeTraitFromInnerClass(innerClass: TypeDef)(using Context): TypeDef =
    val TypeDef(name, tmpl: Template) = innerClass: @unchecked
    val newInnerParents = tmpl.parents.mapConserve(ConcreteParentStripper.apply)
    val tmpl1 = cpy.Template(tmpl)(parents = newInnerParents) // TODO .withType(???)
    val newTrait = cpy.TypeDef(innerClass)(name = SpecializeInlineTraits.newInnerClassName(name), rhs = tmpl1)
    newTrait.symbol.setFlag(Synthetic)
    newTrait
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
      flags = innerClass.symbol.flags & (Private | Protected) | Synthetic,
      info = TypeBounds.upper(upperBound),
      privateWithin = innerClass.symbol.privateWithin,
      coord = innerClass.symbol.coord,
      nestingLevel = innerClass.symbol.nestingLevel,
    ).asType
    TypeDef(newTypeSym)
  end makeTypeFromInnerClass
  */

  private object ConcreteParentStripper extends TreeAccumulator[Tree] {
    def apply(tree: Tree)(using Context): Tree = apply(tree, tree)

    override def apply(x: Tree, tree: Tree)(using Context): Tree = tree match {
      case ident: Ident => ident
      case tpt: TypeTree => tpt
      case _ => foldOver(x, tree)
    }
  }
}

object SpecializeInlineTraits:
  val name: String = "specializeInlineTraits"
  val description: String = "inline the code of inline traits"

  private[transform] def newInnerClassName(name: Name): name.ThisName = name ++ str.INLINE_TRAIT_INNER_CLASS_SUFFIX
