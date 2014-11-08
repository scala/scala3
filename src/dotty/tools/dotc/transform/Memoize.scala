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
  override def treeTransformPhase = thisTransform.next

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (sym.is(Accessor, butNot = NoFieldNeeded))
      if (sym.isGetter) {
        val maybeMutable = if (sym is Stable) EmptyFlags else Mutable
        println(i"add field for $sym")
        val field = ctx.newSymbol(
          owner = ctx.owner,
          name = sym.name.asTermName.fieldName,
          flags = Private | maybeMutable,
          info = sym.info.resultType,
          coord = tree.pos).enteredAfter(thisTransform)
        var fieldInit = tree.rhs.changeOwner(sym, field)
        val fieldDef = ValDef(field, fieldInit)
        val getterDef = cpy.DefDef(tree)(rhs = ref(field))
        Thicket(fieldDef, getterDef)
      }
      else if (sym.isSetter) {
        val Literal(Constant(())) = tree.rhs
        assert(sym.field.exists, i"no field for ${sym.showLocated} in ${sym.owner.info.decls.toList.map{_.showDcl}}%; %")
        val initializer = Assign(ref(sym.field), ref(tree.vparamss.head.head.symbol))
        cpy.DefDef(tree)(rhs = initializer)
      }
      else tree // curiously, some accessors from Scala2 have ' ' suffixes. They count as
                // neither getters nor setters
    else tree
  }
  private val NoFieldNeeded  = Lazy | Deferred | ParamAccessor
}