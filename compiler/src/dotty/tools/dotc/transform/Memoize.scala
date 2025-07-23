package dotty.tools.dotc
package transform

import core.*
import DenotTransformers.*
import Contexts.*
import Phases.*
import SymDenotations.SymDenotation
import Denotations.*
import Symbols.*

import Constants.*
import MegaPhase.*
import NameOps.*
import Flags.*
import Decorators.*
import StdNames.nme

import sjs.JSSymUtils.*

import util.Store
import scala.compiletime.uninitialized

object Memoize {
  val name: String = "memoize"
  val description: String = "add private fields to getters and setters"

  private final class MyState {
    val classesThatNeedReleaseFence = new util.HashSet[Symbol]
  }
}

/** Provides the implementations of all getters and setters, introducing
 *  fields to hold the value accessed by them.
 *  TODO: Make LazyVals a part of this phase?
 *
 *    <accessor> <stable> <mods> def x(): T = e
 *      -->  private val x: T = e
 *           <accessor> <stable> <mods> def x(): T = x
 *
 *    <accessor> <mods> def x(): T = e
 *      -->  private[this] var x: T = e
 *           <accessor> <mods> def x(): T = x
 *
 *    <accessor> <mods> def x_=(y: T): Unit = ()
 *      --> <accessor> <mods> def x_=(y: T): Unit = x = y
 */
class Memoize extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import Memoize.MyState
  import ast.tpd.*

  override def phaseName: String = Memoize.name

  override def description: String = Memoize.description

  private var MyState: Store.Location[MyState] = uninitialized
  private def myState(using Context): MyState = ctx.store(MyState)

  override def initContext(ctx: FreshContext): Unit =
    MyState = ctx.addLocation[MyState]()

  /* Makes sure that, after getters and constructors gen, there doesn't
   * exist non-deferred definitions that are not implemented. */
  override def checkPostCondition(tree: Tree)(using Context): Unit = {
    def errorLackImplementation(t: Tree) = {
      val definingPhase = phaseOf(t.symbol.initial.validFor.firstPhaseId)
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
  override def runsAfter: Set[String] = Set(Mixin.name)

  override def prepareForUnit(tree: Tree)(using Context): Context =
    ctx.fresh.updateStore(MyState, new MyState())

  override def transformTemplate(tree: Template)(using Context): Tree =
    val cls = ctx.owner.asClass
    if myState.classesThatNeedReleaseFence.contains(cls) then
      val releaseFenceCall = ref(defn.staticsMethodRef(nme.releaseFence)).appliedToNone
      cpy.Template(tree)(tree.constr, tree.parents, Nil, tree.self, tree.body :+ releaseFenceCall)
    else
      tree

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val sym = tree.symbol

    def newField = {
      assert(!sym.hasAnnotation(defn.ScalaStaticAnnot))
      val fieldType =
        if (sym.isGetter) sym.info.resultType
        else /*sym.isSetter*/ sym.info.firstParamTypes.head

      newSymbol(
        owner = ctx.owner,
        name  = sym.name.asTermName.fieldName,
        flags = Private | (if (sym.is(StableRealizable)) EmptyFlags else Mutable),
        info  = fieldType,
        coord = tree.span
      ).withAnnotationsCarrying(sym, defn.FieldMetaAnnot, orNoneOf = defn.MetaAnnots)
       .enteredAfter(thisPhase)
    }

    val NoFieldNeeded = Lazy | Deferred | JavaDefined | Inline

    def erasedBottomTree(sym: Symbol) =
      if (sym eq defn.NothingClass) Throw(nullLiteral)
      else if (sym eq defn.NullClass) nullLiteral
      else if (sym eq defn.BoxedUnitClass) ref(defn.BoxedUnit_UNIT)
      else {
        assert(false, s"$sym has no erased bottom tree")
        EmptyTree
      }

    if sym.is(Accessor, butNot = NoFieldNeeded) then
      def adaptToField(field: Symbol, tree: Tree): Tree =
        if (tree.isEmpty) tree else tree.ensureConforms(field.info.widen)

      def isErasableBottomField(field: Symbol, cls: Symbol): Boolean =
        !field.isVolatile
          && ((cls eq defn.NothingClass) || (cls eq defn.NullClass) || (cls eq defn.BoxedUnitClass))
          && !sym.sjsNeedsField

      if sym.isGetter then
        if sym.isConstExprFinalVal then
          // const-expr final vals do not need to be transformed at all, and do not need a field
          tree
        else
          val field = newField.asTerm
          var rhs = tree.rhs.changeOwnerAfter(sym, field, thisPhase)
          if (isWildcardArg(rhs)) rhs = EmptyTree
          val fieldDef = transformFollowing(ValDef(field, adaptToField(field, rhs)))
          val rhsClass = tree.tpt.tpe.widenDealias.classSymbol
          val getterRhs =
            if isErasableBottomField(field, rhsClass) then erasedBottomTree(rhsClass)
            else transformFollowingDeep(ref(field))(using ctx.withOwner(sym))
          val getterDef = cpy.DefDef(tree)(rhs = getterRhs)
          sym.keepAnnotationsCarrying(thisPhase, Set(defn.GetterMetaAnnot))
          Thicket(fieldDef, getterDef)
      else if sym.isSetter then
        if (!sym.is(ParamAccessor)) { val Literal(Constant(())) = tree.rhs: @unchecked } // This is intended as an assertion
        val field = sym.field
        if !field.exists then
          // When transforming the getter, we determined that no field was needed.
          // In that case we can keep the setter as is, with a () rhs.
          tree
        else if field.getter.is(ParamAccessor, butNot = Mutable) then
          // This is a trait setter (because not Mutable) for a param accessor.
          // We must keep the () rhs of the trait setter, otherwise the value
          // inherited from the trait will overwrite the value of the parameter.
          // See tests/run/traitValOverriddenByParamAccessor.scala
          tree
        else
          if !field.is(Mutable) then
            // This is a val mixed in from a trait.
            // We make it mutable, and mark the class as needing a releaseFence() in the constructor
            field.setFlag(Mutable)
            myState.classesThatNeedReleaseFence += sym.owner
          val initializer =
            if isErasableBottomField(field, tree.termParamss.head.head.tpt.tpe.classSymbol)
            then unitLiteral
            else Assign(ref(field), adaptToField(field, ref(tree.termParamss.head.head.symbol)))
          val setterDef = cpy.DefDef(tree)(rhs = transformFollowingDeep(initializer)(using ctx.withOwner(sym)))
          sym.keepAnnotationsCarrying(thisPhase, Set(defn.SetterMetaAnnot))
          setterDef
      else
        // Curiously, some accessors from Scala2 have ' ' suffixes.
        // They count as neither getters nor setters.
        tree
    else
      tree
  }
}
