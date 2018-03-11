package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts.Context
import SymDenotations.SymDenotation
import Types._
import Symbols._
import SymUtils._
import Constants._
import MegaPhase._
import Flags._
import Decorators._
import ValueClasses._

/** Performs the following rewritings for fields of a class:
 *
 *    <mods> val x: T = e
 *      -->  <mods> <stable> <accessor> def x: T = e
 *    <mods> var x: T = e
 *      -->  <mods> <accessor> def x: T = e
 *
 *    <mods> val x: T
 *      -->  <mods> <stable> <accessor> def x: T
 *
 *    <mods> lazy val x: T = e
 *      -->  <mods> <accessor> lazy def x: T =e
 *
 *    <mods> var x: T
 *      -->  <mods> <accessor> def x: T
 *
 *    <mods> non-static <module> val x$ = e
 *      -->  <mods> <module> <accessor> def x$ = e
 *
 *  Omitted from the rewritings are
 *
 *   - private[this] fields in classes (excluding traits, value classes)
 *   - fields generated for static modules (TODO: needed?)
 *   - parameters, static fields, and fields coming from Java
 *
 *  Furthermore, assignments to mutable vars are replaced by setter calls
 *
 *     p.x = e
 *      -->  p.x_=(e)
 *
 *  No fields are generated yet. This is done later in phase Memoize.
 */
class Getters extends MiniPhase with SymTransformer {
  import ast.tpd._

  override def phaseName = "getters"

  override def transformSym(d: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def noGetterNeeded =
      d.is(NoGetterNeeded) ||
      d.initial.asInstanceOf[SymDenotation].isBoth(Private, and = Local) && !d.owner.is(Trait) && !isDerivedValueClass(d.owner) && !d.is(Flags.Lazy) ||
      d.is(Module) && d.isStatic ||
      d.hasAnnotation(defn.ScalaStaticAnnot) ||
      d.isSelfSym
    if (d.isTerm && (d.is(Lazy) || d.owner.isClass) && d.info.isValueType && !noGetterNeeded) {
      val maybeStable = if (d.isStable) Stable else EmptyFlags
      d.copySymDenotation(
        initFlags = d.flags | maybeStable | AccessorCreationFlags,
        info = ExprType(d.info))
    }
    else d
  }
  private val NoGetterNeeded = Method | Param | JavaDefined | JavaStatic

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    if (tree.symbol is Method) DefDef(tree.symbol.asTerm, tree.rhs).withPos(tree.pos) else tree

  override def transformAssign(tree: Assign)(implicit ctx: Context): Tree =
    if (tree.lhs.symbol is Method) tree.lhs.becomes(tree.rhs).withPos(tree.pos) else tree
}
