package dotty.tools.dotc
package transform

import util.Positions._
import MegaPhase.MiniPhase
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, typer._
import TypeUtils._, Flags._
import config.Printers.{ transforms => debug }

/** Check runtime realizability of type test, see the documentation for `Checkable`.
 */
class IsInstanceOfChecker extends MiniPhase {

  import ast.tpd._

  val phaseName = "isInstanceOfChecker"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = {
    def ensureCheckable(qual: Tree, pt: Tree): Tree = {
      if (!Checkable.checkable(qual.tpe, pt.tpe))
         ctx.warning(
           s"the type test for ${pt} cannot be checked at runtime",
           tree.pos
         )

      tree
    }

    tree.fun match {
      case fn: Select if fn.symbol == defn.Any_typeTest =>
        ensureCheckable(fn.qualifier, tree.args.head)
      case fn: Select if fn.symbol == defn.Any_isInstanceOf =>
        ensureCheckable(fn.qualifier, tree.args.head)
      case _ => tree
    }
  }
}

object Checkable {
  import Inferencing._
  import ProtoTypes._

  /** Whether `(x:X).isInstanceOf[P]` can be checked at runtime?
   *
   *  0. if `P` is a singleton type, TRUE
   *  1. if `P` refers to an abstract type member, FALSE
   *  2. if `P = Array[T]`, checkable(E, T) where `E` is the element type of `X`, defaults to `Any`.
   *  3. if `P` is `pre.F[Ts]` and `pre.F` refers to a class which is not `Array`:
   *     (a) replace `Ts` with fresh type variables `Xs`
   *     (b) instantiate `Xs` with the constraint `pre.F[Xs] <:< X`
   *     (c) `pre.F[Xs] <:< P2`, where `P2` is `P` with pattern binder types (e.g., `_$1`)
   *         replaced with `WildcardType`.
   *  4. if `P = T1 | T2` or `P = T1 & T2`, checkable(X, T1) && checkable(X, T2).
   *  5. otherwise, TRUE
   */
  def checkable(X: Type, P: Type)(implicit ctx: Context): Boolean = {
    def Psym = P.dealias.typeSymbol

    def isAbstract = !Psym.isClass

    def replaceBinderMap(implicit ctx: Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef if !tref.typeSymbol.isClass && tref.symbol.is(Case) => WildcardType
        case _ => mapOver(tp)
      }
    }

    def isClassDetermined(tpe: AppliedType)(implicit ctx: Context) = {
      val AppliedType(tycon, args) = tpe
      val tvars = tycon.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds) }
      val P1 = tycon.appliedTo(tvars)

      debug.println("P1 : " + P1)
      debug.println("X : " + X.widen)

      !(P1 <:< X.widen) || {
        // val syms = maximizeType(P1, Psym.pos, fromScala2x = false)
        isFullyDefined(P1, ForceDegree.noBottom)
        val P2   = replaceBinderMap.apply(P)
        val res  = P1 <:< P2
        debug.println("P1: " + P1)
        debug.println("P2: " + P2)
        debug.println("P1 <:< P2 = " + res)
        res
      }
    }

    P match {
      case _: SingletonType     => true
      case defn.ArrayOf(tpT)    =>
        X match {
          case defn.ArrayOf(tpE)   => checkable(tpE, tpT)
          case _                   => checkable(defn.AnyType, tpT)
        }
      case tpe: AppliedType     => !isAbstract && isClassDetermined(tpe)(ctx.fresh.setFreshGADTBounds)
      case AndType(tp1, tp2)    => checkable(X, tp1) && checkable(X, tp2)
      case OrType(tp1, tp2)     => checkable(X, tp1) && checkable(X, tp2)
      case _                    => !isAbstract
    }
  }
}
