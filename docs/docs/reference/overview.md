---
layout: doc-page
title: "Overview"
---

Dotty implements many language changes compared to Scala 2. These changes are currently discussed for inclusion in Scala 3, the new Scala language standard which will be based on the Dotty codebase.

## Goals

The language redesign was guided by three main goals:

 - Strengthen Scala's foundations.
   Make the full programming language compatible with the foundational work on the DOT calculus and apply the lessons learned from that work.
 - Make Scala easier and safer to use. Tame powerful constructs such as implicits to provide a gentler learning curve. Remove warts and puzzlers.
 - Further improve the consistency and expressiveness of Scala's language constructs.

Corresponding to these goals, the language changes fall into seven categories:
(1) Core constructs to strengthen foundations, (2) simplifications and (3) restrictions, to make the language easier and safer to use, (4) dropped constructs to make the language smaller and more regular, (5) changed constructs to remove warts, and increase consistency and usability, (6) new constructs to fill gaps and increase expressiveness, (7) a new, principled approach to meta-programming that replaces today's experimental macros.

## Essential Foundations

These new constructs directly model core features of DOT, higher-kinded types, and the [SI calculus for implicit resolution](https://infoscience.epfl.ch/record/229878/files/simplicitly_1.pdf).

 - [Intersection types](https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html), replacing compound types,
 - [Union types](https://dotty.epfl.ch/docs/reference/new-types/union-types.html),
 - [Type lambdas](https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html),
 replacing encodings using structural types and type projection.
 - [Context Queries](https://dotty.epfl.ch/docs/reference/contextual/implicit-function-types.html)
  (_aka_ implicit function types) offering abstraction over implicit parameters.

## Simplifications

These constructs replace existing constructs with the aim of making the language safer and simpler to use, and to promote uniformity in code style.

 - [Trait Parameters](https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html) replace [early initializers](https://dotty.epfl.ch/docs/reference/dropped-features/early-initializers.html) with a more generally useful construct.
 - [Delegates](https://dotty.epfl.ch/docs/reference/contextual/delegates.html)
   replace implicit objects and defs, focussing on intent over mechanism.
 - [Given Clauses](https://dotty.epfl.ch/docs/reference/contextual/given-clauses.html) replace implicit parameters, avoiding their ambiguities.
 - [Extension Methods](https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html) replace implicit classes with a clearer and simpler mechanism.
 - [Opaque Type Aliases](https://dotty.epfl.ch/docs/reference/other-new-features/opaques.html) replace most uses
   of value classes while guaranteeing absence of boxing.
 - [Toplevel definitions](https://dotty.epfl.ch/docs/reference/dropped-features/package-objects.html) replace package objects, dropping syntactic boilerplate.
 - [Export clauses](https://dotty.epfl.ch/docs/reference/other-new-features/export.html)
 provide a simple and general way to express aggregation, which can replace the
 previous facade pattern of package objects inheriting from classes.
 - [Vararg patterns](https://dotty.epfl.ch/docs/reference/changed-features/vararg-patterns.html) now use the form `: _*` instead of `@ _*`, mirroring vararg expressions,
 - [Creator applications](https://dotty.epfl.ch/docs/reference/other-new-features/creator-applications.html) allow to use simple function call syntax
 instead of `new` expressions. `new` expressions stay around as a fallback for
 the cases where creator applications cannot be used.

With the exception of early initializers and old-style vararg patterns, all superseded constructs continue to be available in Scala 3.0. The plan is to deprecate and phase them out later.

Value classes (superseded by opaque type aliases) ar
e a special case. There are currently no deprecation plans for value classes, since we might want to bring them back in a more general form if they are supported natively by the JVM as is planned by project Valhalla.

## Restrictions

These constructs are restricted to make the language safer.

 - [Implicit Conversions](https://dotty.epfl.ch/docs/reference/contextual/conversions.html): there is only one way to define implicit conversions instead of many, and potentially surprising implicit conversions require a language import.
 - [Delegate Imports](https://dotty.epfl.ch/docs/reference/contextual/import-delegate.html): implicits now require a special form of import, to make the import clearly visible.
 - [Type Projection](https://dotty.epfl.ch/docs/reference/dropped-features/type-projection.html): only classes can be used as prefix `C` of a type projection `C#A`. Type projection on abstract types is no longer supported since it is unsound.
 - [Multiversal Equality](https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality.html) implements an "opt-in" scheme to rule out nonsensical comparisons with `==` and `!=`.
 - [@infix and @alpha](https://github.com/lampepfl/dotty/pull/5975)
 make method application syntax uniform across code bases and require alphanumeric aliases for all symbolic names (proposed, not implemented).

Unrestricted implicit conversions continue to be available in Scala 3.0, but will be deprecated and removed later. Unrestricted versions of the other constructs in the list above are available only under `-language:Scala2`.


## Dropped Constructs

These constructs are proposed to be dropped without a new construct replacing them. The motivation for dropping these constructs is to simplify the language and its implementation.

 - [DelayedInit](https://dotty.epfl.ch/docs/reference/dropped-features/delayed-init.html),
 - [Existential types](https://dotty.epfl.ch/docs/reference/dropped-features/existential-types.html),
 - [Procedure syntax](https://dotty.epfl.ch/docs/reference/dropped-features/procedure-syntax.html),
 - [Class shadowing](https://dotty.epfl.ch/docs/reference/dropped-features/class-shadowing.html),
 - [XML literals](https://dotty.epfl.ch/docs/reference/dropped-features/xml.html),
 - [Symbol literals](https://dotty.epfl.ch/docs/reference/dropped-features/symlits.html),
 - [Auto application](https://dotty.epfl.ch/docs/reference/dropped-features/auto-apply.html),
 - [Weak conformance](https://dotty.epfl.ch/docs/reference/dropped-features/weak-conformance.html),
 - [Compound types](https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html),
 - [Auto tupling](https://github.com/lampepfl/dotty/pull/4311) (implemented, but not merged).

The date when these constructs are dropped varies. The current status is:

 - Not implemented at all:
   - DelayedInit, existential types, weak conformance.
 - Supported under `-language:Scala2`:
   - procedure syntax, class shadowing, symbol literals, auto application, auto tupling in a restricted form.
 - Supported in 3.0, to be deprecated and phased out later:
   - XML literals, compound types.


## Changes

These constructs have undergone changes to make them more regular and useful.

 - [Structural Types](https://dotty.epfl.ch/docs/reference/changed-features/structural-types.html): They now allow pluggable implementations, which greatly increases their usefulness. Some usage patterns are restricted compared to the status quo.
 - [Name-based pattern matching](https://dotty.epfl.ch/docs/reference/changed-features/pattern-matching.html): The existing undocumented Scala 2 implementation has been codified in a slightly simplified form.
 - [Eta expansion](https://dotty.epfl.ch/docs/reference/changed-features/eta-expansion.html) is now performed universally also in the absence of an expected type. The postfix `_` operator is thus made redundant. It will be deprecated and dropped after Scala 3.0.
 - [Implicit Resolution](https://dotty.epfl.ch/docs/reference/changed-features/implicit-resolution.html): The implicit resolution rules have been cleaned up to make them more useful and less surprising. Implicit scope is restricted to no longer include package prefixes.

Most aspects of old-style implicit resolution are still available under `-language:Scala2`. The other changes in this list are applied unconditionally.

## New Constructs

These are additions to the language that make it more powerful or pleasant to use.

 - [Enums](https://dotty.epfl.ch/docs/reference/enums/enums.html) provide concise syntax for enumerations and [algebraic data types](https://dotty.epfl.ch/docs/reference/enums/adts.html).
 - [Parameter Untupling](https://dotty.epfl.ch/docs/reference/other-new-features/parameter-untupling.html) avoids having to use `case` for tupled parameter destructuring.
 - [Dependent Function Types](https://dotty.epfl.ch/docs/reference/new-types/dependent-function-types.html) generalize dependent methods to dependent function values and types.
 - [Polymorphic Function Types](https://github.com/lampepfl/dotty/pull/4672) generalize polymorphic methods to dependent function values and types. _Current status_: There is a proposal, and a prototype implementation, but the implementation has not been finalized or merged yet.
 - [Kind Polymorphism](https://dotty.epfl.ch/docs/reference/other-new-features/kind-polymorphism.html) allows the definition of operators working equally on types and type constructors.

## Meta Programming

The following constructs together aim to put meta programming in Scala on a new basis. So far, meta programming was achieved by a combination of macros and libraries such as Shapeless that were in turn based on some key macros. Current Scala 2 macro mechanisms are a thin veneer on top the current Scala 2 compiler, which makes them fragile and in many cases impossible to port to Scala 3.

It's worth noting that macros were never included in the Scala 2 language specification and were so far made available only under an `-experimental` flag. This has not prevented their widespread usage.

To enable porting most uses of macros, we are experimenting with the advanced language constructs listed below. These designs are more provisional than the rest of the proposed language constructs for Scala 3.0. There might still be some changes until the final release. Stabilizing the feature set needed for meta programming is our first priority.

- [Match Types](https://dotty.epfl.ch/docs/reference/new-types/match-types.html) allow computation on types.
- [Inline](https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html) provides
by itself a straightforward implementation of some simple macros and is at the same time an essential building block for the implementation of complex macros.
- [Quotes and Splices](https://dotty.epfl.ch/docs/reference/metaprogramming/macros.html) provide a principled way to express macros and staging with a unified set of abstractions.
- [Typeclass derivation](https://dotty.epfl.ch/docs/reference/contextual/derivation.html) provides an in-language implementation of the `Gen` macro in Shapeless and other foundational libraries. The new implementation is more robust, efficient and easier to use than the macro.
- [Implicit by-name parameters](https://dotty.epfl.ch/docs/reference/contextual/implicit-by-name-parameters.html) provide a more robust in-language implementation of the `Lazy` macro in Shapeless.
- [Erased Terms](https://dotty.epfl.ch/docs/reference/metaprogramming/erased-terms.html) provide a general mechanism for compile-time-only computations.

## See Also

[A classification of proposed language features](./features-classification.html) is
an expanded version of this page that adds the status (i.e. relative importance to be a part of Scala 3, and relative urgency when to decide this) and expected migration cost
of each language construct.

