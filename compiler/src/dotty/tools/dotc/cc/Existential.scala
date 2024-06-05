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
import NameKinds.exSkolemName
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

  def openExpected(pt: Type)(using Context): Type = pt.dealias match
    case Existential(boundVar, unpacked) =>
      val tm = new IdempotentCaptRefMap:
        val cvar = CaptureSet.Var(ctx.owner)
        def apply(t: Type) = mapOver(t) match
          case t @ CapturingType(parent, refs) if refs.elems.contains(boundVar) =>
            assert(refs.isConst && refs.elems.size == 1, i"malformed existential $t")
            t.derivedCapturingType(parent, cvar)
          case t =>
            t
      openExpected(tm(unpacked))
    case _ => pt

  def toCap(tp: Type)(using Context) = tp.dealias match
    case Existential(boundVar, unpacked) =>
      unpacked.substParam(boundVar, defn.captureRoot.termRef)
    case _ => tp

  /** Replace all occurrences of `cap` in parts of this type by an existentially bound
   *  variable. If there are such occurrences, or there might be in the future due to embedded
   *  capture set variables, create an existential with the variable wrapping the type.
   *  Stop at function or method types since these have been mapped before.
   */
  def mapCap(tp: Type, fail: Message => Unit)(using Context): Type =
    var needsWrap = false

    class Wrap(boundVar: TermParamRef) extends BiTypeMap, ConservativeFollowAliasMap:
      def apply(t: Type) = // go deep first, so that we map parts of alias types before dealiasing
        mapOver(t) match
          case t1: TermRef if t1.isRootCapability =>
            if variance > 0 then
              needsWrap = true
              boundVar
            else
              val varianceStr = if variance < 0 then "contra" else "in"
              fail(em"cap appears in ${varianceStr}variant position in $tp")
              t1
          case t1 @ FunctionOrMethod(_, _) =>
            // These have been mapped before
            t1
          case t1 @ CapturingType(_, _: CaptureSet.Var) =>
            if variance > 0 then needsWrap = true // the set might get a cap later.
            t1
          case t1 =>
            applyToAlias(t, t1)

      lazy val inverse = new BiTypeMap with ConservativeFollowAliasMap:
        def apply(t: Type) = mapOver(t) match
          case t1: TermParamRef if t1 eq boundVar => defn.captureRoot.termRef
          case t1 @ FunctionOrMethod(_, _) => t1
          case t1 => applyToAlias(t, t1)
        def inverse = Wrap.this
        override def toString = "Wrap.inverse"
    end Wrap

    if ccConfig.useExistentials then
      val wrapped = apply(Wrap(_)(tp))
      if needsWrap then wrapped else tp
    else tp
  end mapCap

  def mapCapInResult(tp: Type, fail: Message => Unit)(using Context): Type =
    def mapCapInFinalResult(tp: Type): Type = tp match
      case tp: MethodOrPoly =>
        tp.derivedLambdaType(resType = mapCapInFinalResult(tp.resultType))
      case _ =>
        mapCap(tp, fail)
    tp match
      case tp: MethodOrPoly =>
        mapCapInFinalResult(tp)
      case defn.FunctionNOf(args, res, contextual) =>
        tp.derivedFunctionOrMethod(args, mapCap(res, fail))
      case _ => tp

  def strip(tp: Type)(using Context) = tp match
    case Existential(_, tpunpacked) => tpunpacked
    case _ => tp

  def skolemize(tp: Type)(using Context) = tp.widenDealias match // TODO needed?
    case Existential(boundVar, unpacked) =>
      val skolem = tp match
        case tp: CaptureRef if tp.isTracked => tp
        case _ => newSkolemSym(boundVar.underlying).termRef
      val tm = new IdempotentCaptRefMap:
        var deep = false
        private inline def deepApply(t: Type): Type =
          val saved = deep
          deep = true
          try apply(t) finally deep = saved
        def apply(t: Type) =
          if t eq boundVar then
            if deep then skolem.reach else skolem
          else t match
            case defn.FunctionOf(args, res, contextual) =>
              val res1 = deepApply(res)
              if res1 ne res then defn.FunctionOf(args, res1, contextual)
              else t
            case defn.RefinedFunctionOf(mt) =>
              mt.derivedLambdaType(resType = deepApply(mt.resType))
            case _ =>
              mapOver(t)
      tm(unpacked)
    case _ => tp
  end skolemize

  def newSkolemSym(tp: Type)(using Context): TermSymbol = // TODO needed?
    newSymbol(ctx.owner.enclosingMethodOrClass, exSkolemName.fresh(), Synthetic, tp)
/*
  def fromDepFun(arg: Tree)(using Context): Type = arg.tpe match
    case RefinedType(parent, nme.apply, info: MethodType) if defn.isNonRefinedFunction(parent) =>
      info match
        case info @ MethodType(_ :: Nil)
        if info.paramInfos.head.derivesFrom(defn.Caps_Capability) =>
          apply(ref => info.resultType.substParams(info, ref :: Nil))
        case _ =>
          errorType(em"Malformed existential: dependent function must have a singgle parameter of type caps.Capability", arg.srcPos)
    case _ =>
      errorType(em"Malformed existential: dependent function type expected", arg.srcPos)
*/
  private class PackMap(sym: Symbol, rt: RecType)(using Context) extends DeepTypeMap, IdempotentCaptRefMap:
    def apply(tp: Type): Type = tp match
      case ref: TermRef if ref.symbol == sym => TermRef(rt.recThis, defn.captureRoot)
      case _ => mapOver(tp)

  /** Unpack current type from an existential `rt` so that all references bound by `rt`
   *  are recplaced by `ref`.
   */
  private class OpenMap(rt: RecType, ref: Type)(using Context) extends DeepTypeMap, IdempotentCaptRefMap:
    def apply(tp: Type): Type =
      if isExBound(tp, rt) then ref else mapOver(tp)

  /** Is `tp` a reference to the bound variable of `rt`? */
  private def isExBound(tp: Type, rt: Type)(using Context) = tp match
    case tp @ TermRef(RecThis(rt1), _) => (rt1 eq rt) && tp.symbol == defn.captureRoot
    case _ => false

  /** Open existential, replacing the bund variable by `ref` */
  def open(rt: RecType, ref: Type)(using Context): Type = OpenMap(rt, ref)(rt.parent)

  /** Create an existential type `ex c.<tp>` so that all references to `sym` in `tp`
   *  become references to the existentially bound variable `c`.
   */
  def fromSymbol(tp: Type, sym: Symbol)(using Context): RecType =
    RecType(PackMap(sym, _)(tp))

  def isExistentialMethod(mt: TermLambda)(using Context): Boolean = mt.paramInfos match
    case (info: TypeRef) :: rest => info.symbol == defn.Caps_Exists && rest.isEmpty
    case _ => false

  def isExistentialVar(ref: CaptureRef)(using Context) = ref match
    case ref: TermParamRef => isExistentialMethod(ref.binder)
    case _ => false

  def unapply(tp: Carrier)(using Context): Option[(TermParamRef, Type)] =
    tp.refinedInfo match
      case mt: MethodType
      if isExistentialMethod(mt) && defn.isNonRefinedFunction(tp.parent) =>
        Some(mt.paramRefs.head, mt.resultType)
      case _ => None

  def apply(mk: TermParamRef => Type)(using Context): MethodType =
    MethodType(defn.Caps_Exists.typeRef :: Nil): mt =>
      mk(mt.paramRefs.head)

  /** Create existential if bound variable appear in result */
  def wrap(mk: TermParamRef => Type)(using Context): Type =
    val mt = apply(mk)
    if mt.isResultDependent then mt.toFunctionType() else mt.resType
end Existential
