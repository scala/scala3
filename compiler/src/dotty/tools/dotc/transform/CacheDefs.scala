package dotty.tools.dotc
package transform

import MegaPhase._
import core._
import DenotTransformers.{IdentityDenotTransformer}
import Symbols._, Contexts._, Types._, Flags._, NameOps._, Decorators._
import StdNames.nme
import NameKinds.CacheName
import Constants.Constant
import ast.tpd
import collection.mutable

object CacheDefs {
  val name: String = "cacheDefs"

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
class CacheDefs extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import tpd._

  override def phaseName: String = CacheDefs.name

  override def transformDefDef(tree: DefDef) given (ctx: Context): Tree = {
    val sym = tree.symbol.asTerm
    val isCached = sym.hasAnnotation(defn.CachedAnnot) || {
      sym.info match {
        case _: ExprType if sym.is(Given, butNot = CacheAliasImplicits.NoCacheFlags) =>
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
      val cachedType = sym.info.widenExpr match {
        case mt: MethodType => mt.finalResultType
        case cachedType => cachedType
      }
      val cacheVar = ctx.newSymbol(
        owner = sym.owner,
        name = CacheName(sym.name),
        flags = if (sym.owner.isClass) Private | Local | Mutable else Mutable,
        info = OrType(cachedType, defn.NullType),
        coord = tree.rhs.span
      ).enteredAfter(thisPhase)
      val cacheSetter =
        if (sym.owner.is(Trait))
          ctx.newSymbol(
            owner = sym.owner,
            name = cacheVar.name.setterName,
            flags = cacheVar.flags | Method | Accessor,
            info = MethodType(cachedType :: Nil, defn.UnitType),
            coord = tree.rhs.span
          ).enteredAfter(thisPhase)
        else cacheVar
      val cachedDefs = new mutable.ListBuffer[Tree]()
      cachedDefs += ValDef(cacheVar, nullLiteral)
      if (cacheSetter ne cacheVar)
        cachedDefs += DefDef(cacheSetter, _ => Literal(Constant(())))
      cachedDefs += cpy.DefDef(tree)(rhs =
        Block(
          If(
            ref(cacheVar).select(defn.Any_==).appliedTo(nullLiteral),
            ref(cacheSetter).becomes(tree.rhs),
            unitLiteral) :: Nil,
          ref(cacheVar).cast(cachedType)
        )
      )
      Thicket(cachedDefs.toList)
    }
    else tree
  }
}



