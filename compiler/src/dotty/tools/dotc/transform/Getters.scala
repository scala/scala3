package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Contexts._
import SymDenotations.SymDenotation
import Types._
import Symbols._
import MegaPhase._
import Flags._
import ValueClasses._
import SymUtils._
import NameOps._
import collection.mutable


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
 *  Also, generate setters for fields that are private but not private[this]
 *  The form of a setter is
 *
 *     <mods> def x_=(init: T): Unit = ()
 *
 *  Omitted from the rewritings are
 *
 *   - private[this] fields in classes (excluding traits, value classes)
 *   - fields generated for static modules (TODO: needed?)
 *   - parameters, static fields, and fields coming from Java
 *
 *  The rhs is computed later, in phase Memoize.
 *
 *  Furthermore, assignments to mutable vars with setters are replaced by setter calls
 *
 *     p.x = e
 *      -->  p.x_=(e)
 *
 *  No fields are generated yet. This is done later in phase Memoize.
 *
 *  Also, drop the Local flag from all private[this] and protected[this] members.
 *  This allows subsequent code motions in Flatten.
 */
class Getters extends MiniPhase with SymTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = Getters.name

  override def description: String = Getters.description

  override def transformSym(d: SymDenotation)(using Context): SymDenotation = {
    def noGetterNeeded =
      d.isOneOf(NoGetterNeededFlags) ||
      d.isAllOf(PrivateLocal) && !d.owner.is(Trait) && !isDerivedValueClass(d.owner) && !d.is(Lazy) ||
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
    if (d1.is(Local))
      if (d1 ne d) d1.resetFlag(Local)
      else d1 = d1.copySymDenotation(initFlags = d1.flags &~ Local)
    d1
  }
  private val NoGetterNeededFlags = Method | Param | JavaDefined | JavaStatic

  val newSetters = util.HashSet[Symbol]()

  def ensureSetter(sym: TermSymbol)(using Context) =
    if !sym.setter.exists then
      newSetters += sym.copy(
        name = sym.name.setterName,
        info = MethodType(sym.info.widenExpr :: Nil, defn.UnitType)
      ).enteredAfter(thisPhase)

  override def transformValDef(tree: ValDef)(using Context): Tree =
    val sym = tree.symbol
    if !sym.is(Method) then return tree
    val getterDef = DefDef(sym.asTerm, tree.rhs).withSpan(tree.span)
    if !sym.is(Mutable) then return getterDef
    ensureSetter(sym.asTerm)
    if !newSetters.contains(sym.setter) then return getterDef
    val setterDef = DefDef(sym.setter.asTerm, unitLiteral)
    Thicket(getterDef, setterDef)

  override def transformAssign(tree: Assign)(using Context): Tree =
    val lsym = tree.lhs.symbol.asTerm
    if lsym.is(Method) then
      ensureSetter(lsym)
      tree.lhs.becomes(tree.rhs).withSpan(tree.span)
    else tree
}

object Getters {
  val name: String = "getters"
  val description: String = "replace non-private vals and vars with getter defs"
}
