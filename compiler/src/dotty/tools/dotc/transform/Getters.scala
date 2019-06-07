package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts.Context
import SymDenotations.SymDenotation
import Types._
import Symbols._
import MegaPhase._
import Flags._
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
 *
 *  Also, drop the Local flag from all private[this] and protected[this] members.
 *  This allows subsequent code motions in Flatten.
 */
class Getters extends MiniPhase with SymTransformer {
  import ast.tpd._

  override def phaseName: String = Getters.name

  override def transformSym(d: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def noGetterNeeded =
      d.is(NoGetterNeeded) ||
      d.is(PrivateLocal) && !d.owner.is(Trait) && !isDerivedValueClass(d.owner) && !d.is(Flags.Lazy) ||
      d.is(Module) && d.isStatic ||
      d.hasAnnotation(defn.ScalaStaticAnnot) ||
      d.isSelfSym

    var d1 =
      if (d.isTerm && (d.is(Lazy) || d.owner.isClass) && d.info.isValueType && !noGetterNeeded) {
        val maybeStable = if (d.isStableMember) StableRealizable else EmptyFlags
        d.copySymDenotation(
          initFlags = d.flags | maybeStable | AccessorCreationFlags,
          info = ExprType(d.info))
            // Note: This change will only affect the SymDenotation itself, not
            // SingleDenotations referring to a getter. In this case it does not
            // seem to be a problem since references to a getter don't care whether
            // it's a `T` or a `=> T`
      }
      else d

    // Drop the Local flag from all private[this] and protected[this] members.
    if (d1.is(Local)) {
      if (d1 ne d) d1.resetFlag(Local)
      else d1 = d1.copySymDenotation(initFlags = d1.flags &~ Local)
    }
    d1
  }
  private val NoGetterNeeded = Method | Param | JavaDefined | JavaStatic

  override def transformValDef(tree: ValDef)(implicit ctx: Context): Tree =
    if (tree.symbol is Method) DefDef(tree.symbol.asTerm, tree.rhs).withSpan(tree.span) else tree

  override def transformAssign(tree: Assign)(implicit ctx: Context): Tree =
    if (tree.lhs.symbol is Method) tree.lhs.becomes(tree.rhs).withSpan(tree.span) else tree
}

object Getters {
  val name: String = "getters"
}
