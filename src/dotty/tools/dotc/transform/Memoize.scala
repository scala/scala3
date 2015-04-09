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

  /** Should run after mixin so that fields get generated in the
   *  class that contains the concrete getter rather than the trait
   *  that defines it.
   */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Mixin])

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = tree match {
    case tree: DefDef if !tree.symbol.is(Lazy | Deferred) =>
      assert(!tree.rhs.isEmpty, i"unimplemented: $tree")
    case _ =>
  }

  override def prepareForDefDef(tree: DefDef)(implicit ctx: Context) = {
    val sym = tree.symbol
    if (sym.isGetter && !sym.is(NoFieldNeeded)) {
      // allocate field early so that initializer has the right owner for subsequeny phases in
      // the group.
      val maybeMutable = if (sym is Stable) EmptyFlags else Mutable
      val field = ctx.newSymbol(
        owner = ctx.owner,
        name = sym.name.asTermName.fieldName,
        flags = Private | maybeMutable,
        info = sym.info.resultType,
        coord = tree.pos).enteredAfter(thisTransform)
      tree.rhs.changeOwnerAfter(sym, field, thisTransform)
    }
    this
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    def field = {
      val field = sym.field.asTerm
      assert(field.exists, i"no field for ${sym.showLocated} in ${sym.owner.info.decls.toList.map{_.showDcl}}%; %")
      field
    }
    if (sym.is(Accessor, butNot = NoFieldNeeded))
      if (sym.isGetter) {
        val fieldDef = transformFollowing(ValDef(field, tree.rhs))
        val getterDef = cpy.DefDef(tree)(rhs = transformFollowingDeep(ref(field)))
        Thicket(fieldDef, getterDef)
      }
      else if (sym.isSetter) {
        if (!sym.is(ParamAccessor)) { val Literal(Constant(())) = tree.rhs }
        val initializer = Assign(ref(field), ref(tree.vparamss.head.head.symbol))
        cpy.DefDef(tree)(rhs = transformFollowingDeep(initializer))
      }
      else tree // curiously, some accessors from Scala2 have ' ' suffixes. They count as
                // neither getters nor setters
    else tree
  }
  private val NoFieldNeeded  = Lazy | Deferred | JavaDefined
}
