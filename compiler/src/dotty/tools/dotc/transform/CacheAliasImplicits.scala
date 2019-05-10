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
 *      implicit a for TC = rhs
 *
 *  is expanded before this phase
 *
 *      implicit def a: TC = rhs
 *
 *  It is then expanded further as follows:
 *
 *  1. If `rhs` is a simple name `x` (possibly with a `this.` prefix), leave the definition as is.
 *  2. Otherwise, if `rhs` is a pure path, replace the definition with
 *
 *      implicit val a: TC = rhs
 *
 *  3. Otherwise, if `TC` is a reference type, replace the definition with
 *
 *      private[this] var a$_cache: TC = null
 *      implicit def a: TC = { if (a$_cache == null) a$_cache = rhs; a$_cache }
 *
 *  4. Otherwise `TC` is a value type. Replace the definition with
 *
 *      lazy implicit val a: TC = rhs
 */
class CacheAliasImplicits extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = CacheAliasImplicits.name

  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = {
    val sym = tree.symbol
    sym.info match {
      case ExprType(rhsType) if sym.is(Implied, butNot = CacheAliasImplicits.NoCacheFlags) =>
        // If rhs is a simple TermRef, leave a def.
        tree.rhs.tpe match {
          case TermRef(pre, _) =>
            pre match {
              case NoPrefix => return tree
              case pre: ThisType if pre.cls == ctx.owner.enclosingClass => return tree
              case _ =>
            }
          case _ =>
        }
        def makeVal(additionalFlags: FlagSet) = {
          sym.copySymDenotation(
              initFlags = sym.flags &~ Method | additionalFlags,
              info = rhsType)
            .installAfter(thisPhase)
          cpy.ValDef(tree)(tree.name, tree.tpt, tree.rhs)
        }
        if (isPurePath(tree.rhs)) makeVal(EmptyFlags)
        else if (rhsType.classSymbol.isValueClass ||
                 !erasure(rhsType).typeSymbol.derivesFrom(defn.ObjectClass)) makeVal(Lazy)
        else {
          val cacheFlags = if (ctx.owner.isClass) Private | Local | Mutable else Mutable
          val cacheSym =
            ctx.newSymbol(ctx.owner, CacheName(tree.name), cacheFlags, rhsType, coord = sym.coord)
          if (ctx.owner.isClass) cacheSym.enteredAfter(thisPhase)
          val cacheDef = ValDef(cacheSym, tpd.defaultValue(rhsType))
          val cachingDef = cpy.DefDef(tree)(rhs =
            Block(
              If(
                ref(cacheSym).select(defn.Any_==).appliedTo(nullLiteral),
                Assign(ref(cacheSym), tree.rhs),
                unitLiteral) :: Nil,
              ref(cacheSym)
            )
          )
          Thicket(cacheDef, cachingDef)
        }
      case _ => tree
    }
  }
}



