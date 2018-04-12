package dotty.tools.dotc
package transform

import util.Positions._
import MegaPhase.MiniPhase
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, typer._, ast._, NameKinds._
import TypeUtils._, Flags._
import config.Printers.{ transforms => debug }

/** Check runtime realizability of type test, see the documentation for `Checkable`.
 */
class IsInstanceOfChecker extends MiniPhase {

  import ast.tpd._

  val phaseName = "isInstanceOfChecker"

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = {
    def ensureCheckable(qual: Tree, pt: Tree): Tree = {
      if (!Checkable.checkable(qual.tpe, pt.tpe, tree.pos))
         ctx.warning(
           s"the type test for ${pt.show} cannot be checked at runtime",
           tree.pos
         )

      tree
    }

    tree.fun match {
      case fn: Select
      if fn.symbol == defn.Any_typeTest || fn.symbol == defn.Any_isInstanceOf =>
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
   *  First do the following substitution:
   *  (a) replace `T @unchecked` and pattern binder types (e.g., `_$1`) in P with WildcardType
   *  (b) replace pattern binder types (e.g., `_$1`) in X:
   *      - variance = 1  : hiBound
   *      - variance = -1 : loBound
   *      - variance = 0  : OrType(Any, Nothing) // TODO: use original type param bounds
   *
   *  Then check:
   *
   *  1. if `X <:< P`, TRUE
   *  2. if `P` is a singleton type, TRUE
   *  3. if `P` refers to an abstract type member or type parameter, FALSE
   *  4. if `P = Array[T]`, checkable(E, T) where `E` is the element type of `X`, defaults to `Any`.
   *  5. if `P` is `pre.F[Ts]` and `pre.F` refers to a class which is not `Array`:
   *     (a) replace `Ts` with fresh type variables `Xs`
   *     (b) constrain `Xs` with `pre.F[Xs] <:< X`
   *     (c) instantiate Xs and check `pre.F[Xs] <:< P`
   *  6. if `P = T1 | T2` or `P = T1 & T2`, checkable(X, T1) && checkable(X, T2).
   *  7. if `P` is a refinement type, FALSE
   *  8. otherwise, TRUE
   */
  def checkable(X: Type, P: Type, pos: Position)(implicit ctx: Context): Boolean = {
    def isAbstract(P: Type) = !P.dealias.typeSymbol.isClass
    def isPatternTypeSymbol(sym: Symbol) = !sym.isClass && sym.is(Case)

    def replaceP(tp: Type)(implicit ctx: Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef
        if isPatternTypeSymbol(tref.typeSymbol) => WildcardType
        case AnnotatedType(_, annot)
        if annot.symbol == defn.UncheckedAnnot => WildcardType
        case _ => mapOver(tp)
      }
    }.apply(tp)

    def replaceX(tp: Type)(implicit ctx: Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef
        if isPatternTypeSymbol(tref.typeSymbol) =>
          if (variance == 1) tref.info.hiBound
          else if (variance == -1) tref.info.loBound
          else OrType(defn.AnyType, defn.NothingType)
        case _ => mapOver(tp)
      }
    }.apply(tp)

    /** Approximate type parameters depending on variance */
    def stripTypeParam(tp: Type)(implicit ctx: Context) = new ApproximatingTypeMap {
      def apply(tp: Type): Type = tp match {
        case tp: TypeRef if tp.underlying.isInstanceOf[TypeBounds] =>
          val lo = apply(tp.info.loBound)
          val hi = apply(tp.info.hiBound)
          range(lo, hi)
        case _ =>
          mapOver(tp)
      }
    }.apply(tp)

    def isClassDetermined(X: Type, P: AppliedType)(implicit ctx: Context) = {
      val AppliedType(tycon, _) = P
      val typeLambda = tycon.ensureLambdaSub.asInstanceOf[TypeLambda]
      val tvars = constrained(typeLambda, untpd.EmptyTree, alwaysAddTypeVars = true)._2.map(_.tpe)
      val P1 = tycon.appliedTo(tvars)

      debug.println("P : " + P)
      debug.println("P1 : " + P1)
      debug.println("X : " + X)

      P1 <:< X       // constraint P1

      // use fromScala2x to avoid generating pattern bound symbols
      maximizeType(P1, pos, fromScala2x = true)

      val res = P1 <:< P
      debug.println("P1 : " + P1)
      debug.println("P1 <:< P = " + res)

      res
    }

    def recur(X: Type, P: Type): Boolean = (X <:< P) || (P match {
      case _: SingletonType     => true
      case _: TypeProxy
      if isAbstract(P)          => false
      case defn.ArrayOf(tpT)    =>
        X match {
          case defn.ArrayOf(tpE)   => recur(tpE, tpT)
          case _                   => recur(defn.AnyType, tpT)
        }
      case tpe: AppliedType     =>
        // first try withou striping type parameters for performance
        isClassDetermined(X, tpe)(ctx.fresh.setNewTyperState()) ||
        isClassDetermined(stripTypeParam(X), tpe)(ctx.fresh.setNewTyperState())
      case AndType(tp1, tp2)    => recur(X, tp1) && recur(X, tp2)
      case OrType(tp1, tp2)     => recur(X, tp1) && recur(X, tp2)
      case AnnotatedType(t, _)  => recur(X, t)
      case _: RefinedType       => false
      case _                    => true
    })

    val res = recur(replaceX(X.widen), replaceP(P))

    debug.println(i"checking  ${X.show} isInstanceOf ${P} = $res")

    res
  }
}
