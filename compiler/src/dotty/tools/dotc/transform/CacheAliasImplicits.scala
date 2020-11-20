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

object CacheAliasImplicits {
  val name: String = "cacheAliasImplicits"

  /** Flags that disable caching */
  val NoCacheFlags =
    StableRealizable |  // It's a simple forwarder, leave it as one
    Exported            // Export forwarders are never cached
}

/** This phase ensures that the right hand side of parameterless alias implicits
 *  is cached. It applies to all alias implicits that have neither type parameters
 *  nor a given clause. Example: The alias
 *
 *      TC = rhs
 *
 *  is expanded before this phase to:
 *
 *      implicit def a: TC = rhs
 *
 *  It is then expanded further as follows:
 *
 *  1. If `rhs` is a simple name `x` (possibly with a `this.` prefix) that
 *     refers to a value, leave it as is.
 *
 *  2. Otherwise, replace the definition with
 *
 *      lazy implicit val a: TC = rhs
 */
class CacheAliasImplicits extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = CacheAliasImplicits.name

  private def needsCache(sym: Symbol, rhs: Tree)(using Context): Boolean = rhs.tpe match
    case rhsTpe @ TermRef(NoPrefix, _)
    if rhsTpe.isStable => false
    case rhsTpe @ TermRef(pre: ThisType, _)
    if rhsTpe.isStable && pre.cls == sym.owner.enclosingClass => false
    case rhsTpe: ThisType => false
    case _ => true

  /** Transform
   *
   *    given def x = rhs
   *
   *  to
   *
   *    lazy val x = rhs
   *
   *  unless `rhs` has a stable type and is of one of them forms
   *
   *    this
   *    this.y
   *    y
   *
   *  Parameterless given defs are generated during typeclass derivation.
   */
  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val sym = tree.symbol
    val isCached = !sym.is(Inline) && {
      sym.info match {
        case ExprType(resTpe) if sym.is(Given, butNot = CacheAliasImplicits.NoCacheFlags) =>
          needsCache(sym, tree.rhs)
        case _ => false
      }
    }
    if (isCached) {
      sym.copySymDenotation(
        initFlags = sym.flags &~ Method | Lazy,
        info = sym.info.widenExpr)
      .installAfter(thisPhase)
      cpy.ValDef(tree)(tree.name, tree.tpt, tree.rhs)
    }
    else tree
  }

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
      cpy.DefDef(tree)(tree.name, Nil, Nil, tree.tpt, tree.rhs)
    else tree
}



