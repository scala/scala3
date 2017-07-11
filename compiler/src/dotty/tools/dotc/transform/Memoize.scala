package dotty.tools.dotc
package transform

import core._
import DenotTransformers._
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
import Denotations._
import Types._
import Symbols._
import SymUtils._
import Constants._
import ast.Trees._
import TreeTransforms._
import NameOps._
import Flags._
import Decorators._

/** Provides the implementations of all getters and setters, introducing
 *  fields to hold the value accessed by them.
 *  TODO: Make LazyVals a part of this phase?
 *
 *    <accessor> <stable> <mods> def x(): T = e
 *      -->  private val x: T = e
 *           <accessor> <stable> <mods> def x(): T = x
 *
 *    <accessor> <mods> def x(): T = e
 *      -->  private var x: T = e
 *           <accessor> <mods> def x(): T = x
 *
 *    <accessor> <mods> def x_=(y: T): Unit = ()
 *      --> <accessor> <mods> def x_=(y: T): Unit = x = y
 */
 class Memoize extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName = "memoize"

  /* Makes sure that, after getters and constructors gen, there doesn't
   * exist non-deferred definitions that are not implemented. */
  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    def errorLackImplementation(t: Tree) = {
      val firstPhaseId = t.symbol.initial.validFor.firstPhaseId
      val definingPhase = ctx.withPhase(firstPhaseId).phase.prev
      throw new AssertionError(
        i"Non-deferred definition introduced by $definingPhase lacks implementation: $t")
    }
    tree match {
      case ddef: DefDef
        if !ddef.symbol.is(Deferred) &&
           !ddef.symbol.isConstructor && // constructors bodies are added later at phase Constructors
           ddef.rhs == EmptyTree =>
        errorLackImplementation(ddef)
      case tdef: TypeDef
        if tdef.symbol.isClass && !tdef.symbol.is(Deferred) && tdef.rhs == EmptyTree =>
        errorLackImplementation(tdef)
      case _ =>
    }
    super.checkPostCondition(tree)
  }

  /** Should run after mixin so that fields get generated in the
   *  class that contains the concrete getter rather than the trait
   *  that defines it.
   */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Mixin])

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol

    def newField = {
      assert(!sym.hasAnnotation(defn.ScalaStaticAnnot))
      val fieldType =
        if (sym.isGetter) sym.info.resultType
        else /*sym.isSetter*/ sym.info.firstParamTypes.head

      ctx.newSymbol(
        owner = ctx.owner,
        name  = sym.name.asTermName.fieldName,
        flags = Private | (if (sym is Stable) EmptyFlags else Mutable),
        info  = fieldType,
        coord = tree.pos
      ).withAnnotationsCarrying(sym, defn.FieldMetaAnnot)
       .enteredAfter(thisTransform)
    }

    def addAnnotations(denot: Denotation): Unit =
      denot match {
        case fieldDenot: SymDenotation if sym.annotations.nonEmpty =>
          val cpy = fieldDenot.copySymDenotation()
          cpy.annotations = sym.annotations
          cpy.installAfter(thisTransform)
        case _ => ()
      }

    def removeAnnotations(denot: SymDenotation): Unit =
      if (sym.annotations.nonEmpty) {
        val cpy = sym.copySymDenotation()
        cpy.annotations = Nil
        cpy.installAfter(thisTransform)
      }

    lazy val field = sym.field.orElse(newField).asTerm

    def adaptToField(tree: Tree): Tree =
      if (tree.isEmpty) tree else tree.ensureConforms(field.info.widen)

    val NoFieldNeeded = Lazy | Deferred | JavaDefined | (if (ctx.settings.YnoInline.value) EmptyFlags else Inline)

    def isErasableBottomField(cls: Symbol): Boolean = {
      // TODO: For Scala.js, return false if this field is in a js.Object unless it is an ErasedPhantomClass.
      !field.isVolatile &&
      ((cls eq defn.NothingClass) || (cls eq defn.NullClass) || (cls eq defn.BoxedUnitClass) || (cls eq defn.ErasedPhantomClass))
    }

    def erasedBottomTree(sym: Symbol) = {
      if (sym eq defn.NothingClass) Throw(Literal(Constant(null)))
      else if (sym eq defn.NullClass) Literal(Constant(null))
      else if (sym eq defn.BoxedUnitClass) ref(defn.BoxedUnit_UNIT)
      else if (sym eq defn.ErasedPhantomClass) ref(defn.ErasedPhantom_UNIT)
      else {
        assert(false, sym + " has no erased bottom tree")
        EmptyTree
      }
    }

    if (sym.is(Accessor, butNot = NoFieldNeeded))
      if (sym.isGetter) {
        var rhs = tree.rhs.changeOwnerAfter(sym, field, thisTransform)
        if (isWildcardArg(rhs)) rhs = EmptyTree
        val fieldDef = transformFollowing(ValDef(field, adaptToField(rhs)))
        val rhsClass = tree.tpt.tpe.widenDealias.classSymbol
        val getterRhs =
          if (isErasableBottomField(rhsClass)) erasedBottomTree(rhsClass)
          else transformFollowingDeep(ref(field))(ctx.withOwner(sym), info)
        val getterDef = cpy.DefDef(tree)(rhs = getterRhs)
        addAnnotations(fieldDef.denot)
        removeAnnotations(sym)
        Thicket(fieldDef, getterDef)
      } else if (sym.isSetter) {
        if (!sym.is(ParamAccessor)) { val Literal(Constant(())) = tree.rhs } // This is intended as an assertion
        field.setFlag(Mutable) // Necessary for vals mixed in from Scala2 traits
        if (isErasableBottomField(tree.vparamss.head.head.tpt.tpe.classSymbol)) tree
        else {
          val initializer = Assign(ref(field), adaptToField(ref(tree.vparamss.head.head.symbol)))
          val setterDef = cpy.DefDef(tree)(rhs = transformFollowingDeep(initializer)(ctx.withOwner(sym), info))
          removeAnnotations(sym)
          setterDef
        }
      }
      else tree // curiously, some accessors from Scala2 have ' ' suffixes. They count as
                // neither getters nor setters
    else tree
  }
}
