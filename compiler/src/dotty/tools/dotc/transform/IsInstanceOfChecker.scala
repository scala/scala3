package dotty.tools.dotc
package transform

import util.Positions._
import MegaPhase.MiniPhase
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, typer._
import TypeUtils._, Flags._
import config.Printers.{ transforms => debug }

/** check runtime realizability of type test
 */
class IsInstanceOfChecker extends MiniPhase {

  import ast.tpd._

  val phaseName = "isInstanceOfChecker"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = {
    def ensureCheckable(qual: Tree, pt: Tree): Tree = {
      if (!Checkable.checkable(qual.tpe, pt.tpe))
         ctx.warning(
           s"the type test for ${pt.show} cannot be checked at runtime",
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
   *  The following cases are not checkable at runtime:
   *
   *  1. if `P` refers to an abstract type member
   *  2. if `P` is `pre.F[Ts]` and `pre.F` refers to a class:
   *     (a) replace `Ts` with fresh type variables `Xs`
   *     (b) instantiate `Xs` with the constraint `pre.F[Xs] <:< X`
   *     (c) `pre.F[Xs] <:< P` doesn't hold
   *  3. if `P = T1 | T2` or `P = T1 & T2`, checkable(X, T1) && checkable(X, T2).
   */
  def checkable(X: Type, P: Type)(implicit ctx: Context): Boolean = {
    def Psym = P.dealias.typeSymbol

    def isAbstract = !Psym.isClass

    def isClassDetermined(tpe: AppliedType) = {
      val AppliedType(tycon, args) = tpe
      val tvars = tycon.typeParams.map { tparam => newTypeVar(tparam.paramInfo.bounds) }
      val P2 = tycon.appliedTo(tvars)

      debug.println("P2 : " + P2)
      debug.println("X : " + X)

      !(P2 <:< X.widen) || {
        val syms = maximizeType(P2, Psym.pos, fromScala2x = false)
        val res = P2 <:< P
        debug.println("P2: " + P2.show)
        debug.println("P2 <:< P = " + res)
        res
      }
    }

    P match {
      case tpe: AppliedType  => !isAbstract && isClassDetermined(tpe)
      case AndType(tp1, tp2) => checkable(X, tp1) && checkable(X, tp2)
      case OrType(tp1, tp2)  => checkable(X, tp1) && checkable(X, tp2)
      case _                 => !isAbstract
    }
  }
}