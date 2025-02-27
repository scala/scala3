package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import StdNames.nme
import ast.tpd.*
import Decorators.*
import typer.ErrorReporting.errorType
import Names.TermName
import NameKinds.ExistentialBinderName
import NameOps.isImpureFunction
import CaptureSet.IdempotentCaptRefMap
import reporting.Message
import util.{SimpleIdentitySet, EqHashMap}
import util.Spans.NoSpan

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

In reckeckApply and recheckTypeApply:

  - If an EX is toplevel in the result type, replace its bound variable
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

In box adaptation:

  - Check that existential variables are not boxed or unboxed.

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

Subtype checking algorithm, comparing two capture sets CS1 <:< CS2:

  We need to map the (possibly to-be-added) existentials in CS1 to existentials
  in CS2 so that we can compare them. We use `assocExistentals` for that:
  To map an EX-variable V1 in CS1, pick the last (i.e. outermost, leading to the smallest
  type) EX-variable in `assocExistentials` that has V1 in its possible instances.
  To go the other way (and therby produce a BiTypeMap), map an EX-variable
  V2 in CS2 to the first (i.e. innermost) EX-variable it can be instantiated to.
  If either direction is not defined, we choose a special "bad-existetal" value
  that represents and out-of-scope existential. This leads to failure
  of the comparison.

Existential source syntax:

  Existential types are ususally not written in source, since we still allow the `^`
  syntax that can express most of them more concesely (see below for translation rules).
  But we should also allow to write existential types explicity, even if it ends up mainly
  for debugging. To express them, we use the encoding with `Exists`, so a typical
  expression of an existential would be

      (x: Exists) => A ->{x} B

  Existential types can only at the top level of the result type
  of a function or method.

Restrictions on Existential Types: (to be implemented if we want to
keep the source syntax for users).

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

  /** The (super-) type of existentially bound references */
  type Vble = AnnotatedType

  object Vble:
    def apply(mt: MethodType)(using Context): Vble =
      Fresh.existential(mt)
    def unapply(tp: Vble)(using Context): Option[MethodType] = tp.annot match
      case ann: Fresh.Annot =>
        ann.binder match
          case mt: MethodType =>
            assert(ann.hidden.elems.isEmpty)
            Some(mt)
          case _ => None
      case _ => None

  /** Map top-level free existential variables one-to-one to Fresh instances */
  def toCap(tp: Type)(using Context): Type =
    val subst = new IdempotentCaptRefMap:
      val seen = EqHashMap[Annotation, CaptureRef]()
      var localBinders: SimpleIdentitySet[MethodType] = SimpleIdentitySet.empty

      def apply(t: Type): Type = t match
        case t @ Vble(binder) =>
          if localBinders.contains(binder) then t // keep bound references
          else seen.getOrElseUpdate(t.annot, Fresh(NoSymbol)) // map free references to Fresh()
        case t: MethodType =>
          // skip parameters
          val saved = localBinders
          if t.isFreshBinder then localBinders = localBinders + t
          try t.derivedLambdaType(resType = this(t.resType))
          finally localBinders = saved
        case t: PolyType =>
          // skip parameters
          t.derivedLambdaType(resType = this(t.resType))
        case _ =>
          mapOver(t)

    subst(tp)
  end toCap

  /** Knowing that `tp` is a function type, is it an alias to a function other
   *  than `=>`?
   */
  private def isAliasFun(tp: Type)(using Context) = tp match
    case AppliedType(tycon, _) => !defn.isFunctionSymbol(tycon.typeSymbol)
    case _ => false

  /** Replace all occurrences of `cap` (or fresh) in parts of this type by an existentially bound
   *  variable bound by `mt`.
   *  Stop at function or method types since these have been mapped before.
   */
  def mapCap(tp: Type, mt: MethodType, fail: Message => Unit)(using Context): Type =

    abstract class CapMap extends BiTypeMap:
      override def mapOver(t: Type): Type = t match
        case t @ FunctionOrMethod(args, res) if variance > 0 && !isAliasFun(t) =>
          t // `t` should be mapped in this case by a different call to `mapCap`.
        case t: (LazyRef | TypeVar) =>
          mapConserveSuper(t)
        case _ =>
          super.mapOver(t)

    object toVar extends CapMap:
      private val seen = EqHashMap[CaptureRef, Vble]()

      def apply(t: Type) = t match
        case t: CaptureRef if t.isCapOrFresh =>
          if variance > 0 then
            seen.getOrElseUpdate(t, Vble(mt))
          else
            if variance == 0 then
              fail(em"""$tp captures the root capability `cap` in invariant position""")
            // we accept variance < 0, and leave the cap as it is
            super.mapOver(t)
        case defn.FunctionNOf(args, res, contextual) if t.typeSymbol.name.isImpureFunction =>
          if variance > 0 then
            super.mapOver:
              defn.FunctionNOf(args, res, contextual)
                .capturing(Vble(mt).singletonCaptureSet)
          else mapOver(t)
        case _ =>
          mapOver(t)
        //.showing(i"mapcap $t = $result")
      override def toString = "toVar"

      object inverse extends BiTypeMap:
        def apply(t: Type) = t match
          case t @ Vble(`mt`) =>
            // do a reverse getOrElseUpdate on `seen` to produce the
            // `Fresh` assosicated with `t`
            val it = seen.iterator
            var ref: CaptureRef | Null = null
            while it.hasNext && ref == null do
              val (k, v) = it.next
              if v.annot eq t.annot then ref = k
            if ref == null then
              ref = Fresh(NoSymbol)
              seen(ref) = t
            ref
          case _ => mapOver(t)
        def inverse = toVar.this
        override def toString = "toVar.inverse"
    end toVar

    toVar(tp)
  end mapCap

  /** Map `cap` in function results to fresh existentials */
  def mapCapInResults(fail: Message => Unit)(using Context): TypeMap = new TypeMap with FollowAliasesMap:
    def apply(t: Type): Type = t match
      case defn.RefinedFunctionOf(mt) =>
        val mt1 = apply(mt)
        if mt1 ne mt then mt1.toFunctionType(alwaysDependent = true)
        else t
      case t: MethodType if variance > 0 && t.isFreshBinder =>
        val t1 = mapOver(t).asInstanceOf[MethodType]
        t1.derivedLambdaType(resType = mapCap(t1.resType, t1, fail))
      case CapturingType(parent, refs) =>
        t.derivedCapturingType(this(parent), refs)
      case t: (LazyRef | TypeVar) =>
        mapConserveSuper(t)
      case _ =>
        mapFollowingAliases(t)
  end mapCapInResults

end Existential
