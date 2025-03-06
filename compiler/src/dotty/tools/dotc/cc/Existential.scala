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
object Existential