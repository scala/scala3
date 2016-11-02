package dotty.tools.dotc
package transform

import core._
import DenotTransformers._
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
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
        if !ddef.symbol.is(Deferred) && ddef.rhs == EmptyTree =>
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
      val fieldType =
        if (sym.isGetter) sym.info.resultType
        else /*sym.isSetter*/ sym.info.firstParamTypes.head

      ctx.newSymbol(
        owner = ctx.owner,
        name = sym.name.asTermName.fieldName,
        flags = Private | (if (sym is Stable) EmptyFlags else Mutable),
        info = fieldType,
        coord = tree.pos)
        .withAnnotationsCarrying(sym, defn.FieldMetaAnnot)
        .enteredAfter(thisTransform)
    }

    /** Can be used to filter annotations on getters and setters; not used yet */
    def keepAnnotations(denot: SymDenotation, meta: ClassSymbol) = {
      val cpy = sym.copySymDenotation()
      cpy.filterAnnotations(_.symbol.derivesFrom(meta))
      if (cpy.annotations ne denot.annotations) cpy.installAfter(thisTransform)
    }

    lazy val field = sym.field.orElse(newField).asTerm
    
    def adaptToField(tree: Tree) =
      if (tree.isEmpty) tree else tree.ensureConforms(field.info.widen)
      
    if (sym.is(Accessor, butNot = NoFieldNeeded))
      if (sym.isGetter) {
        def skipBlocks(t: Tree): Tree = t match {
          case Block(_, t1) => skipBlocks(t1)
          case _ => t
        }
        skipBlocks(tree.rhs) match {
          case lit: Literal if sym.is(Final) && isIdempotentExpr(tree.rhs) =>
            // duplicating scalac behavior: for final vals that have rhs as constant, we do not create a field
            // and instead return the value. This seemingly minor optimization has huge effect on initialization
            // order and the values that can be observed during superconstructor call

            // see remark about idempotency in PostTyper#normalizeTree
            cpy.DefDef(tree)(rhs = lit)
          case _ =>
            var rhs = tree.rhs.changeOwnerAfter(sym, field, thisTransform)
            if (isWildcardArg(rhs)) rhs = EmptyTree

            val fieldDef = transformFollowing(ValDef(field, adaptToField(rhs)))
            val getterDef = cpy.DefDef(tree)(rhs = transformFollowingDeep(ref(field))(ctx.withOwner(sym), info))
            Thicket(fieldDef, getterDef)
        }
      } else if (sym.isSetter) {
        if (!sym.is(ParamAccessor)) { val Literal(Constant(())) = tree.rhs } // this is intended as an assertion
        field.setFlag(Mutable) // necessary for vals mixed in from Scala2 traits
        val initializer = Assign(ref(field), adaptToField(ref(tree.vparamss.head.head.symbol)))
        cpy.DefDef(tree)(rhs = transformFollowingDeep(initializer)(ctx.withOwner(sym), info))
      }
      else tree // curiously, some accessors from Scala2 have ' ' suffixes. They count as
                // neither getters nor setters
    else tree
  }
  private val NoFieldNeeded  = Lazy | Deferred | JavaDefined
}
