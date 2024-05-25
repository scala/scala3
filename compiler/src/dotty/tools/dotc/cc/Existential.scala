package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import CaptureSet.IdempotentCaptRefMap
import StdNames.nme

/**

Handling existentials in CC:

In adapt:

  - If an EX is toplevel in expected type, replace with a fresh capture set variable
  - If an EX is toplevel in actual type, find a trackable replacement `x` as follows:
      + If actual type is a trackable ref, pick that
      + Otherwise, create a fresh skolem val symbol with currently enclosing
        method as owner, and use its termRef
    Then,
      + If the EX-bound variable appears only at toplevel, replace it with `x`
      + Otherwise, replace it with a fresh reach capability `x*`.

In avoidance of a type T:

  - Replace all co-variant occurrences of locals variables in T (including locally
    created EX-skolems) with single fresh EX-bound variable, which wraps T.
  - Contravariant occurrences of local variables are approximated by the empty capture set,
    as was the case before.
  - Invariant occurrences of local variables produce errors, as was the case before.
  - Check that no existentially quantified local variable appears under a box.

The reason it is done this way is that it produces the smallest existential type
wrt the existential type ordering shown below. For instance, consider the type

      (A^{x}, B^{y})

where `x` and `y` are local. We widen to

      EX a.(A^{a}, B^{a})

rather than

      EX a.EX b.(A^{a}, A^{b})

In the subtype ordering of existentials the first of these types is a subtype of
the other, but not _vice versa_.

In cv-computation (markFree):

  - Reach capabilities x* of a parameter x cannot appear in the capture set of
    the owning method. They have to be widened to dcs(x), or, where this is not
    possible, it's an error.

In well-formedness checking of explicitly written type T:

  - If T is not the type of a parameter, check that no EX-bound variable appears
    under a box.

Subtype rules

  - new alphabet: existentially bound variables `a`.
  - they can be stored in environments Gamma.
  - they are alpha-renable, usual hygiene conditions apply

      Gamma |- EX a.T <: U
        if Gamma, a |- T <: U

      Gamma |- T <: EX a.U
        if a in Gamma, T <: U

  Note that this is a fairly restrictive ordering. A more permissive ordering would
  allow us to instantiate EX-quantified variables to sets of other EX-quantified
  variables since in the end all we need to do is ensure subcapturing of sets. But
  that would be algorithmically more complicated.

Representation:

  EX a.T[a] is  represented as

      r @ RecType(T[TermRef[r.recThis, caps.cap]]

Subtype checking algorithm, general scheme:

  Maintain two structures in TypeComparer:

    openExistentials: List[RecThis]
    assocExistentials: Map[RecThis, List[RecThis]]

  `openExistentials` corresponds to the list of existential variables stored in the environment.
  `assocExistentials` maps existential variables bound by existentials appearing on the right
  of a subtype judgement to a list of possible associations. Initally this is openExistentials
  at the time when the existential on the right was dropped. It can become a single existential
  when the existentially bound key variable is unified with one of the variables in the
  environment.

Subtype checking algorithm, steps to add for tp1 <:< tp2:

  If tp1 is an existential EX a.tp1a:

      val saved = openExistentials
      openExistentials = tp1.recThis :: openExistentials
      try tp1a <:< tp2
      finally openExistentials = saved

  If tp2 is an existential EX a.tp2a:

      val saved = assocExistentials
      assocExistentials = assocExistentials + (tp2.recThis -> openExistentials
      try tp1 <:< tp2a
      finally assocExistentials = saved

  If tp1 and tp2 are existentially bound variables `TermRef(pre1/pre2: RecThis, cap)`:

      assocExistentials(pre2).contains(pre1) &&
      { assocExistentials(pre2) = List(pre1); true }

Existential source syntax:

  Existential types are ususally not written in source, since we still allow the `^`
  syntax that can express most of them more concesely (see below for translation rules).
  But we should also allow to write existential types explicity, even if it ends up mainly
  for debugging. To express them, we add the following trait definition in the caps object:

      trait Exists[X]

  A typical expression of an existential is then

      Exists[(x: Capability) => A ->{x} B]

  Existential types are expanded at Typer to the RecType syntax presented above. It is checked
  that the type argument is a dependent function type with one argument of type `Capability` and
  that this argument is used only in capture sets of the result type.

  Existential types can only appear at the top-level of _legal existential scopes_. These are:

    - The type of a binding: i.e a type of a parameter or val, a result type of a def, or
      a self type of a class.
    - The type of a type ascription in an expression or pattern
    - The argument and result types of a function.

Expansion of ^:

  We drop `cap` as a capture reference, but keep the unqualified `^` syntax.
  This now expands to existentials. The translation treats each legal existential scope
  separately. If existential scopes nest, the inner ones are translated first.

  The expansion algorithm is then defined as follows:

    1. In an existential scope, replace every occurrence of ^ with a fresh existentially
        bound variable and quantify over all variables such introduced.

    2. After this step, type aliases are expanded. If aliases have aliases in arguments,
        the outer alias is expanded before the aliases in the arguments. Each time an alias
        is expanded that reveals a `^`, apply step (1).

    3. The algorithm ends when no more alieases remain to be expanded.

  ^ captures outside an existential scope or the right hand side of a type alias (e.g. in
  a class parent) are not allowed.

  Examples:

    - `A => B` is an alias type that expands to `(A -> B)^`, which expands to `EX c. A ->{c} B`.

    - `Iterator[A => B]` expands to `EX c. Iterator[A ->{c} B]`

    - `A -> B^` expands to `A -> EX c.B^{c}`.

    - If we define `type Fun[T] = A -> T`, then `Fun[B^]` expands to `EX c.Fun[B^{c}]`, which
      dealiases to `EX c.A -> B^{c}`.

    - If we define

        type F = A -> Fun[B^]

      then the type alias expands to

        type F = A -> EX c.A -> B^{c}

      since the result type of the RHS is a legal existential scope.
*/
object Existential:

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

  def unapply(rt: RecType)(using Context): Option[Type] =
    if isCaptureChecking && rt.parent.existsPart(isExBound(_, rt))
    then Some(rt.parent)
    else None

end Existential
