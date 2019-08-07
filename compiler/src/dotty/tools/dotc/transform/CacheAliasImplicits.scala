package dotty.tools.dotc
package transform

import MegaPhase._
import core.DenotTransformers.{IdentityDenotTransformer}
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.StdNames.nme
import core.NameKinds.CacheName
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
 *      given a as TC = rhs
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

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {
    val sym = tree.symbol
    val isCached = sym.is(Inline) && {
      sym.info match {
        case ExprType(resTpe) if sym.is(Given, butNot = CacheAliasImplicits.NoCacheFlags) =>
          tree.rhs.tpe match {
            case rhsTpe @ TermRef(NoPrefix, _)
            if rhsTpe.isStable => false
            case rhsTpe @ TermRef(pre: ThisType, _)
            if rhsTpe.isStable && pre.cls == sym.owner.enclosingClass => false
            case _ => true
          }
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
}



