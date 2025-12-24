---
layout: doc-page
title: "Capture Checking Internals"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/internals.html
---

The capture checker is architected as a propagation constraint solver, which runs as a separate phase after type-checking and some initial transformations.

Constraint variables stand for unknown capture sets. A constraint variable is introduced

 - for every part of a previously inferred type,
 - for the accessed references of every method, class, anonymous function, or by-name argument,
 - for the parameters passed in a class constructor call.

Capture sets in explicitly written types are treated as constants (before capture checking, such sets are simply ignored).

The capture checker essentially rechecks the program with the usual typing rules. Every time a subtype requirement between capturing types is checked, this translates to a subcapturing test on capture sets. If the two sets are constant, this is simply a yes/no question, where a no will produce an error message.

If the lower set `C₁` of a comparison `C₁ <: C₂` is a variable, the set `C₂` is recorded
as a _superset_ of `C₁`. If the upper set `C₂` is a variable, the elements of `C₁` are _propagated_ to `C₂`. Propagation of an element `x` to a set `C` means that `x` is included as an element in `C`, and it is also propagated
to all known supersets of `C`. If such a superset is a constant, it is checked that `x` is included in it. If that's not the case, the original comparison `C₁ <: C₂` has no solution and an error is reported.

The type checker also performs various maps on types, for instance when substituting actual argument types for formal parameter types in dependent functions, or mapping
member types with "as-seen-from" in a selection. Maps keep track of the variance
of positions in a type. The variance is initially covariant, it flips to
contravariant in function parameter positions, and can be either covariant,
contravariant, or nonvariant in type arguments, depending on the variance of
the type parameter.

When capture checking, the same maps are also performed on capture sets. If a capture set is a constant, its elements (which are capabilities) are mapped as regular types. If the result of such a map is not a capability, the result is approximated according to the variance of the type. A covariant approximation replaces a type by its capture set.
A contravariant approximation replaces it with the empty capture set. A nonvariant
approximation replaces the enclosing capturing type with a range of possible types
that gets propagated and resolved further out.

When a mapping `m` is performed on a capture set variable `C`, a new variable `Cm` is created that contains the mapped elements and that is linked with `C`. If `C` subsequently acquires further elements through propagation, these are also propagated to `Cm` after being transformed by the `m` mapping. `Cm` also gets the same supersets as `C`, mapped again using `m`.

One interesting aspect of the capture checker concerns the implementation of capture tunneling. The [foundational theory](https://infoscience.epfl.ch/record/290885) on which capture checking is based makes tunneling explicit through so-called _box_ and
_unbox_ operations. Boxing hides a capture set and unboxing recovers it. The capture checker inserts virtual box and unbox operations based on actual and expected types similar to the way the type checker inserts implicit conversions. When capture set variables are first introduced, any capture set in a capturing type that is an instance of a type parameter instance is marked as "boxed". A boxing operation is
inserted if the expected type of an expression is a capturing type with
a boxed capture set variable. The effect of the insertion is that any references
to capabilities in the boxed expression are forgotten, which means that capture
propagation is stopped. Dually, if the actual type of an expression has
a boxed variable as capture set, an unbox operation is inserted, which adds all
elements of the capture set to the environment.

Boxing and unboxing has no runtime effect, so the insertion of these operations is  only simulated; the only visible effect is the retraction and insertion
of variables in the capture sets representing the environment of the currently checked expression.

The `-Ycc-debug` option provides some insight into the workings of the capture checker.
When it is turned on, boxed sets are marked explicitly and capture set variables are printed with an ID and some information about their provenance. For instance, the string `{f, xs}33M5V` indicates a capture set
variable that is known to hold elements `f` and `xs`. The variable's ID is `33`. The `M`
indicates that the variable was created through a mapping from a variable with ID `5`. The latter is a regular variable, as indicated
 by `V`.

Generally, the string following the capture set consists of alternating numbers and letters where each number gives a variable ID and each letter gives the provenance of the variable. Possible letters are

 - `V` : a regular variable,
 - `M` : a variable resulting from a _mapping_ of the variable indicated by the string to the right,
 - `B` : similar to `M` but where the mapping is a _bijection_,
 - `F` : a variable resulting from _filtering_ the elements of the variable indicated by the string to the right,
 - `I` : a variable resulting from an _intersection_ of two capture sets,
 - `D` : a variable resulting from the set _difference_ of two capture sets.
 - `R` : a regular variable that _refines_ a class parameter, so that the capture
         set of a constructor argument is known in the class instance type.

At the end of a compilation run, `-Ycc-debug` will print all variable dependencies of variables referred to in previous output. Here is an example:
```
Capture set dependencies:
  {}2V                 ::
  {}3V                 ::
  {}4V                 ::
  {f, xs}5V            :: {f, xs}31M5V, {f, xs}32M5V
  {f, xs}31M5V         :: {xs, f}
  {f, xs}32M5V         ::
```
This section lists all variables that appeared in previous diagnostics and their dependencies, recursively. For instance, we learn that

 - variables 2, 3, 4 are empty and have no dependencies,
 - variable `5` has two dependencies: variables `31` and `32` which both result from mapping variable `5`,
 - variable `31` has a constant fixed superset `{xs, f}`
 - variable `32` has no dependencies.



