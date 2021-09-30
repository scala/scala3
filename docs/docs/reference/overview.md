---
title: "Overview"
type: chapter
num: 1
next-page: /scala3/reference/new-types
---

Scala 3 implements many language changes and improvements over Scala 2.
In this reference, we discuss design decisions and present important differences compared to Scala 2.

## Goals

The language redesign was guided by three main goals:

- Strengthen Scala's foundations.
  Make the full programming language compatible with the foundational work on the
  [DOT calculus](https://infoscience.epfl.ch/record/227176/files/soundness_oopsla16.pdf)
  and apply the lessons learned from that work.
- Make Scala easier and safer to use.
  Tame powerful constructs such as implicits to provide a gentler learning curve. Remove warts and puzzlers.
- Further improve the consistency and expressiveness of Scala's language constructs.

Corresponding to these goals, the language changes fall into seven categories:
(1) Core constructs to strengthen foundations, (2) simplifications and (3) [restrictions](#restrictions), to make the language easier and safer to use, (4) [dropped constructs](#dropped-constructs) to make the language smaller and more regular, (5) [changed constructs](#changes) to remove warts, and increase consistency and usability, (6) [new constructs](#new-constructs) to fill gaps and increase expressiveness, (7) a new, principled approach to metaprogramming that replaces [Scala 2 experimental macros](https://docs.scala-lang.org/overviews/macros/overview.html).

## Essential Foundations

These new constructs directly model core features of DOT, higher-kinded types, and the [SI calculus for implicit resolution](https://infoscience.epfl.ch/record/229878/files/simplicitly_1.pdf).

- [Intersection types](new-types/intersection-types.html), replacing compound types,
- [Union types](new-types/union-types.html),
- [Type lambdas](new-types/type-lambdas.html), replacing encodings using structural types and type projection.
- [Context functions](contextual/context-functions.html), offering abstraction over given parameters.

## Simplifications

These constructs replace existing constructs with the aim of making the language safer and simpler to use, and to promote uniformity in code style.

- [Trait parameters](other-new-features/trait-parameters.html)
  replace [early initializers](dropped-features/early-initializers.html) with a more generally useful construct.
- [Given instances](contextual/givens.html)
  replace implicit objects and defs, focussing on intent over mechanism.
- [Using clauses](contextual/using-clauses.html)
  replace implicit parameters, avoiding their ambiguities.
- [Extension methods](contextual/extension-methods.html)
  replace implicit classes with a clearer and simpler mechanism.
- [Opaque type aliases](other-new-features/opaques.html)
  replace most uses of value classes while guaranteeing absence of boxing.
- [Top-level definitions](dropped-features/package-objects.html)
  replace package objects, dropping syntactic boilerplate.
- [Export clauses](other-new-features/export.html)
  provide a simple and general way to express aggregation, which can replace
  the previous facade pattern of package objects inheriting from classes.
- [Vararg splices](changed-features/vararg-splices.html)
  now use the form `xs*` in function arguments and patterns instead of `xs: _*` and `xs @ _*`,
- [Universal apply methods](other-new-features/creator-applications.html)
  allow using simple function call syntax instead of `new` expressions. `new` expressions stay around
  as a fallback for the cases where creator applications cannot be used.

With the exception of [early initializers](dropped-features/early-initializers.html) and old-style vararg patterns, all superseded constructs continue to be available in Scala 3.0. The plan is to deprecate and phase them out later.

Value classes (superseded by opaque type aliases) are a special case. There are currently no deprecation plans for value classes, since we might bring them back in a more general form if they are supported natively by the JVM as is planned by [project Valhalla](https://openjdk.java.net/projects/valhalla/).

## Restrictions

These constructs are restricted to make the language safer.

- [Implicit Conversions](contextual/conversions.html):
  there is only one way to define implicit conversions instead of many, and potentially surprising implicit conversions require a language import.
- [Given Imports](contextual/given-imports.html):
  implicits now require a special form of import, to make the import clearly visible.
- [Type Projection](dropped-features/type-projection.html):
  only classes can be used as prefix `C` of a type projection `C#A`. Type projection on abstract types is no longer supported since it is unsound.
- [Multiversal Equality](contextual/multiversal-equality.html):
  implement an "opt-in" scheme to rule out nonsensical comparisons with `==` and `!=`.
- [infix](changed-features/operators.html):
  make method application syntax uniform across code bases.

Unrestricted implicit conversions continue to be available in Scala 3.0, but will be deprecated and removed later. Unrestricted versions of the other constructs in the list above are available only under `-source 3.0-migration`.

## Dropped Constructs

These constructs are proposed to be dropped without a new construct replacing them. The motivation for dropping these constructs is to simplify the language and its implementation.

- [DelayedInit](dropped-features/delayed-init.html),
- [Existential types](dropped-features/existential-types.html),
- [Procedure syntax](dropped-features/procedure-syntax.html),
- [Class shadowing](dropped-features/class-shadowing.html),
- [XML literals](dropped-features/xml.html),
- [Symbol literals](dropped-features/symlits.html),
- [Auto application](dropped-features/auto-apply.html),
- [Weak conformance](dropped-features/weak-conformance.html),
- Compound types (replaced by [Intersection types](new-types/intersection-types.html)),
- [Auto tupling](https://github.com/lampepfl/dotty/pull/4311) (implemented, but not merged).

The date when these constructs are dropped varies. The current status is:

- Not implemented at all:
    - DelayedInit, existential types, weak conformance.
- Supported under `-source 3.0-migration`:
    - procedure syntax, class shadowing, symbol literals, auto application, auto tupling in a restricted form.
- Supported in 3.0, to be deprecated and phased out later:
    - [XML literals](dropped-features/xml.html), compound types.

## Changes

These constructs have undergone changes to make them more regular and useful.

- [Structural Types](changed-features/structural-types.html):
  They now allow pluggable implementations, which greatly increases their usefulness. Some usage patterns are restricted compared to the status quo.
- [Name-based pattern matching](changed-features/pattern-matching.html):
  The existing undocumented Scala 2 implementation has been codified in a slightly simplified form.
- [Automatic Eta expansion](changed-features/eta-expansion.html):
  Eta expansion is now performed universally also in the absence of an expected type. The postfix `_` operator is thus made redundant. It will be deprecated and dropped after Scala 3.0.
- [Implicit Resolution](changed-features/implicit-resolution.html):
  The implicit resolution rules have been cleaned up to make them more useful and less surprising. Implicit scope is restricted to no longer include package prefixes.

Most aspects of old-style implicit resolution are still available under `-source 3.0-migration`. The other changes in this list are applied unconditionally.

## New Constructs

These are additions to the language that make it more powerful or pleasant to use.

- [Enums](enums/enums.html) provide concise syntax for enumerations and [algebraic data types](enums/adts.html).
- [Parameter untupling](other-new-features/parameter-untupling.html) avoids having to use `case` for tupled parameter destructuring.
- [Dependent function types](new-types/dependent-function-types.html) generalize dependent methods to dependent function values and types.
- [Polymorphic function types](new-types/polymorphic-function-types.html) generalize polymorphic methods to polymorphic function values and types.
  _Current status_: There is a proposal and a merged prototype implementation, but the implementation has not been finalized (it is notably missing type inference support).
- [Kind polymorphism](other-new-features/kind-polymorphism.html) allows the definition of operators working equally on types and type constructors.
- [`@targetName` annotations](other-new-features/targetName.html) make it easier to interoperate with code written in other languages and give more flexibility for avoiding name clashes.

## Metaprogramming

The following constructs together aim to put metaprogramming in Scala on a new basis. So far, metaprogramming was achieved by a combination of macros and libraries such as [Shapeless](https://github.com/milessabin/shapeless) that were in turn based on some key macros. Current Scala 2 macro mechanisms are a thin veneer on top the current Scala 2 compiler, which makes them fragile and in many cases impossible to port to Scala 3.

It's worth noting that macros were never included in the [Scala 2 language specification](https://scala-lang.org/files/archive/spec/2.13/) and were so far made available only under an `-experimental` flag. This has not prevented their widespread usage.

To enable porting most uses of macros, we are experimenting with the advanced language constructs listed below. These designs are more provisional than the rest of the proposed language constructs for Scala 3.0. There might still be some changes until the final release. Stabilizing the feature set needed for metaprogramming is our first priority.

- [Match Types](new-types/match-types.html)
  allow computation on types.
- [Inline](metaprogramming/inline.html)
  provides by itself a straightforward implementation of some simple macros and is at the same time an essential building block for the implementation of complex macros.
- [Quotes and Splices](metaprogramming/macros.html)
  provide a principled way to express macros and staging with a unified set of abstractions.
- [Type class derivation](contextual/derivation.html)
  provides an in-language implementation of the `Gen` macro in Shapeless and other foundational libraries. The new implementation is more robust, efficient and easier to use than the macro.
- [By-name context parameters](contextual/by-name-context-parameters.html)
  provide a more robust in-language implementation of the `Lazy` macro in [Shapeless](https://github.com/milessabin/shapeless).

## See Also

[A classification of proposed language features](./features-classification.html) is
an expanded version of this page that adds the status (i.e. relative importance to be a part of Scala 3, and relative urgency when to decide this) and expected migration cost
of each language construct.
