package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers.{IdentityDenotTransformer}
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.StdNames.nme
import core.Constants.Constant
import core.Decorators._
import core.TypeErasure.erasure
import ast.tpd

object UncacheGivenAliases:
  val name: String = "uncacheGivenAliases"
  val description: String = "avoid caching RHS of simple parameterless given aliases"

/** This phase optimizes alias givens represented as lazy vals to be uncached
 *  if that does not change runtime behavior. A definition does not need to be
 *  cached if its right hand side has a stable type and is of one of them forms
 *
 *    this
 *    this.y
 *    y
 */
class UncacheGivenAliases extends MiniPhase with IdentityDenotTransformer:
  thisPhase =>
  import tpd._

  override def phaseName: String = UncacheGivenAliases.name

  override def description: String = UncacheGivenAliases.description

  private def needsCache(sym: Symbol, rhs: Tree)(using Context): Boolean = rhs.tpe match
    case rhsTpe @ TermRef(NoPrefix, _)
    if rhsTpe.isStable => false
    case rhsTpe @ TermRef(pre: ThisType, _)
    if rhsTpe.isStable && pre.cls == sym.owner.enclosingClass => false
    case rhsTpe: ThisType => false
    case _ => true

  /** Transform
   *
   *    lazy given val x = rhs
   *
   *  to
   *
   *    def x = rhs
   *
   *  provided `rhs` has a stable type and is of one of them forms
   *
   *    this
   *    this.y
   *    y
   */
  override def transformValDef(tree: ValDef)(using Context): Tree =
    val sym = tree.symbol
    if sym.isAllOf(Given, Lazy) && !needsCache(sym, tree.rhs) then
      sym.copySymDenotation(
        initFlags = sym.flags &~ Lazy | Method,
        info = ExprType(sym.info))
      .installAfter(thisPhase)
      cpy.DefDef(tree)(tree.name, Nil, tree.tpt, tree.rhs)
    else tree
end UncacheGivenAliases


