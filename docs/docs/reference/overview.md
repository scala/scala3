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

 - [Intersection types](new-types/intersection-types.md), replacing compound types,
 - [Union types](new-types/union-types.md),
 - [Type lambdas](new-types/type-lambdas.md),
 replacing encodings using structural types and type projection.
 - [Context Queries](contextual/implicit-function-types.md)
  (_aka_ implicit function types) offering abstraction over implicit parameters.

## Simplifications

These constructs replace existing constructs with the aim of making the language safer and simpler to use, and to promote uniformity in code style.

 - [Trait Parameters](other-new-features/trait-parameters.md) replace [early initializers](dropped-features/early-initializers.md) with a more generally useful construct.
 - [Delegates](contextual/delegates.md)
   replace implicit objects and defs, focussing on intent over mechanism.
 - [Given Clauses](contextual/given-clauses.md) replace implicit parameters, avoiding their ambiguities.
 - [Extension Methods](contextual/extension-methods.md) replace implicit classes with a clearer and simpler mechanism.
 - [Opaque Type Aliases](other-new-features/opaques.md) replace most uses
   of value classes while guaranteeing absence of boxing.
 - [Toplevel definitions](dropped-features/package-objects.md) replace package objects, dropping syntactic boilerplate.
 - [Export clauses](other-new-features/export.md)
 provide a simple and general way to express aggregation, which can replace the
 previous facade pattern of package objects inheriting from classes.
 - [Vararg patterns](changed-features/vararg-patterns.md) now use the form `: _*` instead of `@ _*`, mirroring vararg expressions,
 - [Creator applications](other-new-features/creator-applications.md) allow to use simple function call syntax
 instead of `new` expressions. `new` expressions stay around as a fallback for
 the cases where creator applications cannot be used.

With the exception of early initializers and old-style vararg patterns, all superseded constructs continue to be available in Scala 3.0. The plan is to deprecate and phase them out later.

Value classes (superseded by opaque type aliases) ar
e a special case. There are currently no deprecation plans for value classes, since we might want to bring them back in a more general form if they are supported natively by the JVM as is planned by project Valhalla.

## Restrictions

These constructs are restricted to make the language safer.

 - [Implicit Conversions](contextual/conversions.md): there is only one way to define implicit conversions instead of many, and potentially surprising implicit conversions require a language import.
 - [Delegate Imports](contextual/import-delegate.md): implicits now require a special form of import, to make the import clearly visible.
 - [Type Projection](dropped-features/type-projection.md): only classes can be used as prefix `C` of a type projection `C#A`. Type projection on abstract types is no longer supported since it is unsound.
 - [Multiversal Equality](contextual/multiversal-equality.md) implements an "opt-in" scheme to rule out nonsensical comparisons with `==` and `!=`.
 - [@infix and @alpha](https://github.com/lampepfl/dotty/pull/5975)
 make method application syntax uniform across code bases and require alphanumeric aliases for all symbolic names (proposed, not implemented).

Unrestricted implicit conversions continue to be available in Scala 3.0, but will be deprecated and removed later. Unrestricted versions of the other constructs in the list above are available only under `-language:Scala2`.


## Dropped Constructs

These constructs are proposed to be dropped without a new construct replacing them. The motivation for dropping these constructs is to simplify the language and its implementation.

 - [DelayedInit](dropped-features/delayed-init.md),
 - [Existential types](dropped-features/existential-types.md),
 - [Procedure syntax](dropped-features/procedure-syntax.md),
 - [Class shadowing](dropped-features/class-shadowing.md),
 - [XML literals](dropped-features/xml.md),
 - [Symbol literals](dropped-features/symlits.md),
 - [Auto application](dropped-features/auto-apply.md),
 - [Weak conformance](dropped-features/weak-conformance.md),
 - [Compound types](new-types/intersection-types.md),
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

 - [Structural Types](changed-features/structural-types.md): They now allow pluggable implementations, which greatly increases their usefulness. Some usage patterns are restricted compared to the status quo.
 - [Name-based pattern matching](changed-features/pattern-matching.md): The existing undocumented Scala 2 implementation has been codified in a slightly simplified form.
 - [Eta expansion](changed-features/eta-expansion.md) is now performed universally also in the absence of an expected type. The postfix `_` operator is thus made redundant. It will be deprecated and dropped after Scala 3.0.
 - [Implicit Resolution](changed-features/implicit-resolution.md): The implicit resolution rules have been cleaned up to make them more useful and less surprising. Implicit scope is restricted to no longer include package prefixes.

Most aspects of old-style implicit resolution are still available under `-language:Scala2`. The other changes in this list are applied unconditionally.

## New Constructs

These are additions to the language that make it more powerful or pleasant to use.

 - [Enums](enums/enums.md) provide concise syntax for enumerations and [algebraic data types](enums/adts.md).
 - [Parameter Untupling](other-new-features/parameter-untupling.md) avoids having to use `case` for tupled parameter destructuring.
 - [Dependent Function Types](new-types/dependent-function-types.md) generalize dependent methods to dependent function values and types.
 - [Polymorphic Function Types](https://github.com/lampepfl/dotty/pull/4672) generalize polymorphic methods to dependent function values and types. _Current status_: There is a proposal, and a prototype implementation, but the implementation has not been finalized or merged yet.
 - [Kind Polymorphism](other-new-features/kind-polymorphism.md) allows the definition of operators working equally on types and type constructors.

## Meta Programming

The following constructs together aim to put meta programming in Scala on a new basis. So far, meta programming was achieved by a combination of macros and libraries such as Shapeless that were in turn based on some key macros. Current Scala 2 macro mechanisms are a thin veneer on top the current Scala 2 compiler, which makes them fragile and in many cases impossible to port to Scala 3.

It's worth noting that macros were never included in the Scala 2 language specification and were so far made available only under an `-experimental` flag. This has not prevented their widespread usage.

To enable porting most uses of macros, we are experimenting with the advanced language constructs listed below. These designs are more provisional than the rest of the proposed language constructs for Scala 3.0. There might still be some changes until the final release. Stabilizing the feature set needed for meta programming is our first priority.

- [Match Types](new-types/match-types.md) allow computation on types.
- [Inline](metaprogramming/inline.md) provides
by itself a straightforward implementation of some simple macros and is at the same time an essential building block for the implementation of complex macros.
- [Quotes and Splices](metaprogramming/macros.md) provide a principled way to express macros and staging with a unified set of abstractions.
- [Typeclass derivation](contextual/derivation.md) provides an in-language implementation of the `Gen` macro in Shapeless and other foundational libraries. The new implementation is more robust, efficient and easier to use than the macro.
- [Implicit by-name parameters](contextual/implicit-by-name-parameters.md) provide a more robust in-language implementation of the `Lazy` macro in Shapeless.
- [Erased Terms](metaprogramming/erased-terms.md) provide a general mechanism for compile-time-only computations.

## See Also

[A classification of proposed language features](./features-classification.md) is
an expanded version of this page that adds the status (i.e. relative importance to be a part of Scala 3, and relative urgency when to decide this) and expected migration cost
of each language construct.

