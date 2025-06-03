Handling invariant capture set variables
========================================

This note gives a high-level overview of constraints and constraint solving in Scala's capture checker. It then describes a proposal to improve a current problem area: the handling of invariantly mapped capture set variables.

A capture checking constraint consists of subcapturing constraints between
capture sets. Capture sets can be variable or constant. A capture set variable is either defined by itself,
or is the result of a mapping of a type map.
Depending on what part of an original type created a map result, the set `v` is classified as invariant, covariant, or contravariant.

We can describe the syntax of capture set constraints like this:
```
Capture set a, b, c = ac     // constant, aliases bv, cc
                    | av     // variable, aliases bv, cv

Typemap          tm = B      // bijective on capabilities
                    | F      // arbitrary

Constraint     C    = a <: b     // simple constraint
                    | a = B(b)   // bimap constraint
                    | a >: F(b)  // covariant map constraint
                    | a <: F(b)  // contravariant map constraint
                    | a = F(b)   // invariant map constraint
```
Variables appearing in the left hand side of a map constraint are called
map results. In a constraint set, each map result variable appears on the left hand side of only one map constraint, the one which defines it.
We distinguish two kinds of type maps, bijective maps `B` and general maps `F`. General maps are capture-monotonic: if `A <: B`
then the capture set of `F(A)` subcaptures the capture set of `F(B)`. All considered type maps map `cap` to `cap`.

Examples of bijective maps are substitutions of parameters of one method type
for another, or mapping local method parameter symbols to method parameters.
Examples of monotonic maps are substitutions of argument types for parameters,
avoidance, or as-seen-from.

Bijective map constraints define variables exactly. General map constraints come in three different forms. In the form `a >: F(b)`, `a` is bounded from
below by the map's result. Such constraints are created when a part
of a type appearing in covariant position is mapped. Consequently, variables
bound that way are called _covariant_. _Contravariant_ variables are defined
by constraints of the `a <: F(b)`. Such constraints are created when a part
of a type appearing in contravariant position is mapped. Finally, _invariant_ variables are defined by constraints of the `a = F(b)`, which are created when a part of a type appearing in invariant position is mapped.

A solution to a subcapturing constraint set consists of an assignment of capture set constants to capture set variables, so that the resulting constraint set is true. The capture checker uses an incremental propagation-based
solver where capture set variables record which references they are known to contain. When new elements
are added to a set or a new relationship between capture sets is added, elements are propagated until
a new partial solution is found, or a contradiction is detected. A contradiction arises when an attempt
is made to add an element to a constant set that is not accounted for by the set.

The propagation actions depend on the kind of constraint and whether a new capture set `c` is propagated
to the left part `a` or the right part `b` of the constraint. In the cases where the constraint
involves a general mapping `F`, the mapping `F(r)` of a capability `r` is in general a type, not
a capability or capture set. In this case we use `F(r).CS` for the capture set of the type
`F(r)`. Likewise, if `c` is a capture set, then `F(c)` might be a capture set `c'` if `F` maps all
capabilities in `c` to capabilities. But in general it will be a set of types, and `F(c).CS`
denotes the set of capture sets of these types. Furthermore, we use `c.super` for the capture set
of the definition of the reference `c` (as it shows up in rule (sc-var)).

The propagation rules are summarized as follows:

```
Constraint     new relation  propagation                 remark
---------------------------------------------------------------
 a <: b         c <: a        c <: b
                c <: b        none

 a = B(b)       c <: a        B^-1(c) <: b
                c <: b        B(c) <: a

 a >: F(b)      c <: a        none
                c <: b        F(c).CS <: a

 a <: F(b)      c <: a        c <: b        if F(c) = c   (1)
                              c.super <: a  otherwise
                c <: b        none

 a = F(b)       c <: a        c <: b        if F(c) = c   (1)
                              c.super <: a  otherwise
                c <: b        c' <: a       if F(c) = c'
                              F(c).CS? <: a otherwise     (2)
```
Remarks:

(1) If `F(c) = c`, we solve by adding `c` also to `b`. Otherwise
   we try again with `c.super <: a` instead of `c <: a`. At some point
   this will succeed since we assume that for all maps `F`, `F(cap) = cap`.
   This is clearly sound, but loses completeness since there might be
   other, smaller solutions for the constraint. But with `F` not being
   bijective, we'd have to try all possible capture sets to find
   a suitable set that we could add to `b` so that `c` subcaptures the mapped set.

(2) is the hard case. If the target set `a` appears invariantly, and `F(x)` is a type, we can use neither the lower bound set `{}` nor the upper bound set `F(x).CS`. Previously, we addressed this problem by making sure case (2) could not arise using the _range_ mechanism, which is also employed for regular type inference. To explain, let's say we try to map some type `t` where `F` is not defined on `t` but we know lower and upper approximations `tl` and `tu`. If `t` appears in
co- or contravariant positions, we can return `tu` or `tl`, respectively. But if `t`
appears in invariant position, we return `Range(tl, tu)`. This is not a type, but a type interval
which will be mapped itself, and will be resolved to one of its bounds at the first
point further out where the variance is positive or negative. There's also the case
where a Range appears in argument position of a parameterized class type, where we can
map it to the wildcard type `? >: tl <: tu`. We used the same mechanism for capture
set inference, by converting invariant occurrences of mapped type arguments `t^v` to wildcard types `? >: t^v1 <: t^v2` proactively, splitting the variable `v` to `v1` and `v2`, so that all capture set constraints occurred at co- or contravariant positions.

Unfortunately this splitting has bad consequences for type inference, since a wildcard
is usually not compatible with a single expected type. So we would like to find another
technique for solving this issue.

The idea is to use a technique similar to ranges directly on the capture sets. This means
that we now have a new class of elements of capture sets standing for references that might or might not be in the set. We use `{x?, ...}` as notation for a reference `x` that might or might not be an element of the enclosing capture set. Like `Range`, the notation is purely internal, we
never write it in a source program.

We now describe how to handle the missing case (2) above. Assume `F(x)` is not a tracked reference, its image `a` is invariant and its capture set `cfr` is `{x_1,...,x_n}` where `n > 0`. In this case we add to `a` the _maybe references_ `x_1?, ..., x_n?`. Generalized to sets, `c <: b` where
`F(c) != c` leads to `F(c).CS?` to be added to `a`. Here, `CS?` is the set consisting of all references `x` in `cs` converted to maybe references `x?`.

These references behave as follows:

 - For unions, `x` subsumes `x?`, so adding both `x` and `x?` to a set means
   only `x` needs to be addded.

 - For further propagation of elements up the chain in e.g. `a <: b`, a maybe reference `x?` behaves like `x`. That is, since the reference could be in the set we have to propagate it upwards to be sound.

 - For propagations from lower sets, a maybe reference is treated as if it was missing. For instance, if we have `a <: b`, `b` contains `x?` and we propagate `x` from `a` to `b`, we need to add `x` as usual to `b`, which might imply back-proagation in the case `b` is an image of a type map `F`.

 - For mappings, maybe references are handled depending on the variance of the
   image set. Assume we add `x?` to `b`. If `a >: F(b)` this amounts
   to adding `F(x).CS` to `a`. If `a` is contravariant, this amounts to doing nothing. If `a` is invariant, we add `F(x).CS?` to `a`. If
   `CS` is a capture set variable, it's "maybe" status has to be recorded and all elements this set acquires in the future also have to be converted to maybe references.



