package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import CaptureSet.IdempotentCaptRefMap
import StdNames.nme
import ast.tpd.*
import Decorators.*
import typer.ErrorReporting.errorType
import Names.TermName
import NameKinds.ExistentialBinderName
import NameOps.isImpureFunction
import reporting.Message

/**

Handling existentials in CC:

 - We generally use existentials only in function and method result types
 - All occurrences of an EX-bound variable appear co-variantly in the bound type

In Setup:

 - Convert occurrences of `cap` in function results to existentials. Precise rules below.
 - Conversions are done in two places:

    + As part of mapping from local types of parameters and results to infos of methods.
      The local types just use `cap`, whereas the result type in the info uses EX-bound variables.
    + When converting functions or methods appearing in explicitly declared types.
      Here again, we only replace cap's in fucntion results.

 - Conversion is done with a BiTypeMap in `Existential.mapCap`.

In adapt:

  - If an EX is toplevel in actual type, replace its bound variable
    occurrences with `cap`.

Level checking and avoidance:

  - Environments, capture refs, and capture set variables carry levels

    + levels start at 0
    + The level of a block or template statement sequence is one higher than the level of
      its environment
    + The level of a TermRef is the level of the environment where its symbol is defined.
    + The level of a ThisType is the level of the statements of the class to which it beloongs.
    + The level of a TermParamRef is currently -1 (i.e. TermParamRefs are not yet checked using this system)
    + The level of a capture set variable is the level of the environment where it is created.

  - Variables also carry info whether they accept `cap` or not. Variables introduced under a box
    don't, the others do.

  - Capture set variables do not accept elements of level higher than the variable's level
  - We use avoidance to heal such cases: If the level-incorrect ref appears
    + covariantly: widen to underlying capture set, reject if that is cap and the variable does not allow it
    + contravariantly: narrow to {}
    + invarianty: reject with error

In cv-computation (markFree):

  - Reach capabilities x* of a parameter x cannot appear in the capture set of
    the owning method. They have to be widened to dcs(x), or, where this is not
    possible, it's an error.

In well-formedness checking of explicitly written type T:

  - If T is not the type of a parameter, check that no cap occurrence or EX-bound variable appears
    under a box.

Subtype rules

  - new alphabet: existentially bound variables `a`.
  - they can be stored in environments Gamma.
  - they are alpha-renable, usual hygiene conditions apply

      Gamma |- EX a.T <: U
        if Gamma, a |- T <: U

      Gamma |- T <: EX a.U
        if exists capture set C consisting of capture refs and ex-bound variables
        bound in Gamma such that  Gamma |- T <: [a := C]U

Representation:

  EX a.T[a] is represented as a dependent function type

      (a: Exists) => T[a]]

  where Exists is defined in caps like this:

      sealed trait Exists extends Capability

  The defn.RefinedFunctionOf extractor will exclude existential types from
  its results, so only normal refined functions match.

  Let `boundvar(ex)` be the TermParamRef defined by the existential type `ex`.

Subtype checking algorithm, general scheme:

  Maintain two structures in TypeComparer:

    openExistentials: List[TermParamRef]
    assocExistentials: Map[TermParamRef, List[TermParamRef]]

  `openExistentials` corresponds to the list of existential variables stored in the environment.
  `assocExistentials` maps existential variables bound by existentials appearing on the right
  to the value of `openExistentials` at the time when the existential on the right was dropped.

Subtype checking algorithm, steps to add for tp1 <:< tp2:

  If tp1 is an existential EX a.tp1a:

      val saved = openExistentials
      openExistentials = boundvar(tp1) :: openExistentials
      try tp1a <:< tp2
      finally openExistentials = saved

  If tp2 is an existential EX a.tp2a:

      val saved = assocExistentials
      assocExistentials = assocExistentials + (boundvar(tp2) -> openExistentials)
      try tp1 <:< tp2a
      finally assocExistentials = saved

  If tp2 is an existentially bound variable:
    assocExistentials(tp2).isDefined
    && (assocExistentials(tp2).contains(tp1) || tp1 is not existentially bound)

Existential source syntax:

  Existential types are ususally not written in source, since we still allow the `^`
  syntax that can express most of them more concesely (see below for translation rules).
  But we should also allow to write existential types explicity, even if it ends up mainly
  for debugging. To express them, we use the encoding with `Exists`, so a typical
  expression of an existential would be

      (x: Exists) => A ->{x} B

  Existential types can only at the top level of the result type
  of a function or method.

Restrictions on Existential Types:

    - An existential capture ref must be the only member of its set. This is
      intended to model the idea that existential variables effectibely range
      over capture sets, not capture references. But so far our calculus
      and implementation does not yet acoommodate first-class capture sets.
    - Existential capture refs must appear co-variantly in their bound type

  So the following would all be illegal:

      EX x.C^{x, io}            // error: multiple members
      EX x.() => EX y.C^{x, y}  // error: multiple members
      EX x.C^{x} ->{x} D        // error: contra-variant occurrence
      EX x.Set[C^{x}]           // error: invariant occurrence

Expansion of ^:

  We expand all occurrences of `cap` in the result types of functions or methods
  to existentially quantified types. Nested scopes are expanded before outer ones.

  The expansion algorithm is then defined as follows:

    1. In a result type, replace every occurrence of ^ with a fresh existentially
       bound variable and quantify over all variables such introduced.

    2. After this step, type aliases are expanded. If aliases have aliases in arguments,
       the outer alias is expanded before the aliases in the arguments. Each time an alias
       is expanded that reveals a `^`, apply step (1).

    3. The algorithm ends when no more alieases remain to be expanded.

  Examples:

    - `A => B` is an alias type that expands to `(A -> B)^`, therefore
      `() -> A => B` expands to `() -> EX c. A ->{c} B`.

    - `() => Iterator[A => B]` expands to `() => EX c. Iterator[A ->{c} B]`

    - `A -> B^` expands to `A -> EX c.B^{c}`.

    - If we define `type Fun[T] = A -> T`, then `() -> Fun[B^]` expands to `() -> EX c.Fun[B^{c}]`, which
      dealiases to `() -> EX c.A -> B^{c}`.

    - If we define

        type F = A -> Fun[B^]

      then the type alias expands to

        type F = A -> EX c.A -> B^{c}
*/
object Existential:

  type Carrier = RefinedType

  def unapply(tp: Carrier)(using Context): Option[(TermParamRef, Type)] =
    tp.refinedInfo match
      case mt: MethodType
      if isExistentialMethod(mt) && defn.isNonRefinedFunction(tp.parent) =>
        Some(mt.paramRefs.head, mt.resultType)
      case _ => None

  /** Create method type in the refinement of an existential type */
  private def exMethodType(using Context)(
      mk: TermParamRef => Type,
      boundName: TermName = ExistentialBinderName.fresh()
    ): MethodType =
    MethodType(boundName :: Nil)(
      mt => defn.Caps_Exists.typeRef :: Nil,
      mt => mk(mt.paramRefs.head))

  /** Create existential */
  def apply(mk: TermParamRef => Type)(using Context): Type =
    exMethodType(mk).toFunctionType(alwaysDependent = true)

  /** Create existential if bound variable appears in result of `mk` */
  def wrap(mk: TermParamRef => Type)(using Context): Type =
    val mt = exMethodType(mk)
    if mt.isResultDependent then mt.toFunctionType() else mt.resType

  extension (tp: Carrier)
    def derivedExistentialType(core: Type)(using Context): Type = tp match
      case Existential(boundVar, unpacked) =>
        if core eq unpacked then tp
        else apply(bv => core.substParam(boundVar, bv))
      case _ =>
        core

  /** Map top-level existentials to `cap`. Do the same for existentials
   *  in function results if all preceding arguments are known to be always pure.
   */
  def toCap(tp: Type)(using Context): Type = tp.dealiasKeepAnnots match
    case Existential(boundVar, unpacked) =>
      val transformed = unpacked.substParam(boundVar, defn.captureRoot.termRef)
      transformed match
        case FunctionOrMethod(args, res @ Existential(_, _))
        if args.forall(_.isAlwaysPure) =>
          transformed.derivedFunctionOrMethod(args, toCap(res))
        case _ =>
          transformed
    case tp1 @ CapturingType(parent, refs) =>
      tp1.derivedCapturingType(toCap(parent), refs)
    case tp1 @ AnnotatedType(parent, ann) =>
      tp1.derivedAnnotatedType(toCap(parent), ann)
    case _ => tp

  /** Map existentials at the top-level and in all nested result types to `cap`
   */
  def toCapDeeply(tp: Type)(using Context): Type = tp.dealiasKeepAnnots match
    case Existential(boundVar, unpacked) =>
      toCapDeeply(unpacked.substParam(boundVar, defn.captureRoot.termRef))
    case tp1 @ FunctionOrMethod(args, res) =>
      val tp2 = tp1.derivedFunctionOrMethod(args, toCapDeeply(res))
      if tp2 ne tp1 then tp2 else tp
    case tp1 @ CapturingType(parent, refs) =>
      tp1.derivedCapturingType(toCapDeeply(parent), refs)
    case tp1 @ AnnotatedType(parent, ann) =>
      tp1.derivedAnnotatedType(toCapDeeply(parent), ann)
    case _ => tp

  /** Knowing that `tp` is a function type, is an alias to a function other
   *  than `=>`?
   */
  private def isAliasFun(tp: Type)(using Context) = tp match
    case AppliedType(tycon, _) => !defn.isFunctionSymbol(tycon.typeSymbol)
    case _ => false

  /** Replace all occurrences of `cap` in parts of this type by an existentially bound
   *  variable. If there are such occurrences, or there might be in the future due to embedded
   *  capture set variables, create an existential with the variable wrapping the type.
   *  Stop at function or method types since these have been mapped before.
   */
  def mapCap(tp: Type, fail: Message => Unit)(using Context): Type =
    var needsWrap = false

    abstract class CapMap extends BiTypeMap:
      override def mapOver(t: Type): Type = t match
        case t @ FunctionOrMethod(args, res) if variance > 0 && !isAliasFun(t) =>
          t // `t` should be mapped in this case by a different call to `mapCap`.
        case Existential(_, _) =>
          t
        case t: (LazyRef | TypeVar) =>
          mapConserveSuper(t)
        case _ =>
          super.mapOver(t)

    class Wrap(boundVar: TermParamRef) extends CapMap:
      def apply(t: Type) = t match
        case t: TermRef if t.isRootCapability =>
          if variance > 0 then
            needsWrap = true
            boundVar
          else
            if variance == 0 then
              fail(em"""$tp captures the root capability `cap` in invariant position""")
            // we accept variance < 0, and leave the cap as it is
            super.mapOver(t)
        case t @ CapturingType(parent, refs: CaptureSet.Var) =>
          if variance > 0 then needsWrap = true
          super.mapOver(t)
        case _ =>
          mapOver(t)
        //.showing(i"mapcap $t = $result")

      lazy val inverse = new BiTypeMap:
        def apply(t: Type) = t match
          case t: TermParamRef if t eq boundVar => defn.captureRoot.termRef
          case _ => mapOver(t)
        def inverse = Wrap.this
        override def toString = "Wrap.inverse"
    end Wrap

    if ccConfig.useExistentials then
      val wrapped = apply(Wrap(_)(tp))
      if needsWrap then wrapped else tp
    else tp
  end mapCap

  def mapCapInResults(fail: Message => Unit)(using Context): TypeMap = new:

    def mapFunOrMethod(tp: Type, args: List[Type], res: Type): Type =
      val args1 = atVariance(-variance)(args.map(this))
      val res1 = res match
        case res: MethodType => mapFunOrMethod(res, res.paramInfos, res.resType)
        case res: PolyType => mapFunOrMethod(res, Nil, res.resType) // TODO: Also map bounds of PolyTypes
        case _ => mapCap(apply(res), fail)
      tp.derivedFunctionOrMethod(args1, res1)

    def apply(t: Type): Type = t match
      case FunctionOrMethod(args, res) if variance > 0 && !isAliasFun(t) =>
        mapFunOrMethod(t, args, res)
      case CapturingType(parent, refs) =>
        t.derivedCapturingType(this(parent), refs)
      case Existential(_, _) =>
        t
      case t: (LazyRef | TypeVar) =>
        mapConserveSuper(t)
      case _ =>
        mapOver(t)
  end mapCapInResults

  /** Is `mt` a method represnting an existential type when used in a refinement? */
  def isExistentialMethod(mt: TermLambda)(using Context): Boolean = mt.paramInfos match
    case (info: TypeRef) :: rest => info.symbol == defn.Caps_Exists && rest.isEmpty
    case _ => false

  /** Is `ref` this an existentially bound variable? */
  def isExistentialVar(ref: CaptureRef)(using Context) = ref match
    case ref: TermParamRef => isExistentialMethod(ref.binder)
    case _ => false

  def isBadExistential(ref: CaptureRef) = ref match
    case ref: TermParamRef => ref.paramName == nme.OOS_EXISTENTIAL
    case _ => false

  def badExistential(using Context): TermParamRef =
    exMethodType(identity, nme.OOS_EXISTENTIAL).paramRefs.head

end Existential
