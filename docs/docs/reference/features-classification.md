---
layout: doc-page
title: A Classification of Proposed Language Features
date: April 6, 2019
author: Martin Odersky
---

This document provides an overview of the constructs proposed for Scala 3 with the aim to facilitate the discussion what to include and when to include it. It classifies features into eight groups: (1) essential foundations, (2) simplifications, (3) restrictions, (4) dropped features, (5) changed features, (6) new features, (7) features oriented towards meta-programming with the aim to replace existing macros, and (8) changes to type checking and inference.

Each group contains sections classifying the status (i.e. relative importance to be a part of Scala 3, and relative urgency when to decide this) and the migration cost
of the constructs in it.

The current document reflects the state of things as of April, 2019. It will be updated to reflect any future changes in that status.

## Essential Foundations

These new constructs directly model core features of DOT, higher-kinded types, and the [SI calculus for implicit resolution](https://infoscience.epfl.ch/record/229878/files/simplicitly_1.pdf).

 - [Intersection types](https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html), replacing compound types,
 - [Union types](https://dotty.epfl.ch/docs/reference/new-types/union-types.html),
 - [Type lambdas](https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html),
 replacing encodings using structural types and type projection.
 - [Context Queries](https://dotty.epfl.ch/docs/reference/contextual/implicit-function-types.html)
  (_aka_ implicit function types) offering abstraction over given parameters.

**Status: essential**

These are essential core features of Scala 3. Without them, Scala 3 would be a completely different language, with different foundations.

**Migration cost: none to low**

Since these are additions, there's generally no migration cost for old code. An exception are intersection types which replace compound types with slightly cleaned-up semantics. But few programs would be affected by this change.

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
 - [Export clauses](https://dotty.epfl.ch/docs/reference/new-features/export.html)
 provide a simple and general way to express aggregation, which can replace the
 previous facade pattern of package objects inheriting from classes.
 - [Vararg patterns](https://dotty.epfl.ch/docs/reference/changed-features/vararg-patterns.html) now use the form `: _*` instead of `@ _*`, mirroring vararg expressions,
 - [Creator applications](https://dotty.epfl.ch/docs/reference/other-new-features/creator-applications.html) allow to use simple function call syntax
 instead of `new` expressions. `new` expressions stay around as a fallback for
 the cases where creator applications cannot be used.

With the exception of early initializers and old-style vararg patterns, all superseded constructs continue to be available in Scala 3.0. The plan is to deprecate and phase them out later.

Value classes (superseded by opaque type aliases) are a special case. There are currently no deprecation plans for value classes, since we might want to bring them back in a more general form if they are supported natively by the JVM as is planned by project Valhalla.

**Status: bimodal: now or never / can delay**

These are essential simplifications. If we decide to adopt them, we should do it for 3.0. Otherwise we are faced with the awkward situation that the Scala 3 documentation has to describe an old feature that will be replaced or superseded by a simpler one in the future.

On the other hand, we need to decide now only about the new features in this list. The decision to drop the superseded features can be delayed. Of course, adopting a new feature without deciding to drop the superseded feature will make the language larger.

**Migration cost: moderate**

For the next several versions, old features will remain available and deprecation and rewrite techniques can make any migration effort low and gradual.


## Restrictions

These constructs are restricted to make the language safer.

 - [Implicit Conversions](https://dotty.epfl.ch/docs/reference/contextual/conversions.html): there is only one way to define implicit conversions instead of many, and potentially surprising implicit conversions require a language import.
 - [Delegate Imports](https://dotty.epfl.ch/docs/reference/contextual/import-delegate.html): implicits now require a special form of import, to make the import clearly visible.
 - [Type Projection](https://dotty.epfl.ch/docs/reference/dropped-features/type-projection.html): only classes can be used as prefix `C` of a type projection `C#A`. Type projection on abstract types is no longer supported since it is unsound.
 - [Multiversal Equality](https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality.html) implements an "opt-in" scheme to rule out nonsensical comparisons with `==` and `!=`.
 - [@infix and @alpha](https://github.com/lampepfl/dotty/pull/5975)
 make method application syntax uniform across code bases and require alphanumeric aliases for all symbolic names (proposed, not implemented).

Unrestricted implicit conversions continue to be available in Scala 3.0, but will be deprecated and removed later. Unrestricted versions of the other constructs in the list above are available only under `-language:Scala2`.

**Status: now or never**

These are essential restrictions. If we decide to adopt them, we should do it for 3.0. Otherwise we are faced with the awkward situation that the Scala 3 documentation has to describe a feature that will be restricted in the future.

**Migration cost: low to high**

 - _low_: multiversal equality rules out code that is nonsensical, so any rewrites required by its adoption should be classified as bug fixes.
 - _moderate_: Restrictions to implicits can be accommodated by straightforward rewriting.
 - _high_: Unrestricted type projection cannot always rewritten directly since it is unsound in general.

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

**Status: mixed**

Currently unimplemented features would require considerable implementation effort which would in most cases make the compiler more buggy and fragile and harder to understand. If we do not decide to drop them, they will probably show up as "not yet implemented" in the Scala 3.0 release.

Currently implemented features could stay around indefinitely. Updated docs may simply ignore them, in the expectation that they might go away eventually. So the decision about their removal can be delayed.

**Migration cost: moderate to high**

Dropped features require rewrites to avoid their use in programs. These rewrites can sometimes be automatic (e.g. for procedure syntax, symbol literals, auto application)
and sometimes need to be manual (e.g. class shadowing, auto tupling). Sometimes the rewrites would have to be non-local, affecting use sites as well as definition sites (e.g., in the case of DelayedInit, unless we find a solution).

## Changes

These constructs have undergone changes to make them more regular and useful.

 - [Structural Types](https://dotty.epfl.ch/docs/reference/changed-features/structural-types.html): They now allow pluggable implementations, which greatly increases their usefulness. Some usage patterns are restricted compared to the status quo.
 - [Name-based pattern matching](https://dotty.epfl.ch/docs/reference/changed-features/pattern-matching.html): The existing undocumented Scala 2 implementation has been codified in a slightly simplified form.
 - [Eta expansion](https://dotty.epfl.ch/docs/reference/changed-features/eta-expansion.html) is now performed universally also in the absence of an expected type. The postfix `_` operator is thus made redundant. It will be deprecated and dropped after Scala 3.0.
 - [Implicit Resolution](https://dotty.epfl.ch/docs/reference/changed-features/implicit-resolution.html): The implicit resolution rules have been cleaned up to make them more useful and less surprising. Implicit scope is restricted to no longer include package prefixes.

Most aspects of old-style implicit resolution are still available under `-language:Scala2`. The other changes in this list are applied unconditionally.

**Status: strongly advisable**

The features have been implemented in their new form in Scala 3.0's compiler. They provide clear improvements in simplicity and functionality compared to the status quo. Going back would require significant implementation effort for a net loss of functionality.

**Migration cost: low to high**

Only a few programs should require changes, but some necessary changes might be non-local (as in the case of restrictions to implicit scope).

## New Constructs

These are additions to the language that make it more powerful or pleasant to use.

 - [Enums](https://dotty.epfl.ch/docs/reference/enums/enums.html) provide concise syntax for enumerations and [algebraic data types](https://dotty.epfl.ch/docs/reference/enums/adts.html).
 - [Parameter Untupling](https://dotty.epfl.ch/docs/reference/other-new-features/parameter-untupling.html) avoids having to use `case` for tupled parameter destructuring.
 - [Dependent Function Types](https://dotty.epfl.ch/docs/reference/new-types/dependent-function-types.html) generalize dependent methods to dependent function values and types.
 - [Polymorphic Function Types](https://github.com/lampepfl/dotty/pull/4672) generalize polymorphic methods to dependent function values and types. _Current status_: There is a proposal, and a prototype implementation, but the implementation has not been finalized or merged yet.
 - [Kind Polymorphism](https://dotty.epfl.ch/docs/reference/other-new-features/kind-polymorphism.html) allows the definition of operators working equally on types and type constructors.

**Status: mixed**

Enums offer an essential simplification of fundamental use patterns, so they should be adopted for Scala 3.0. Auto-parameter tupling is a very small change that removes some awkwardness, so it might as well be adopted now. The other features constitute more specialized functionality which could be introduced in later versions. On the other hand, except for polymorphic function types they are all fully implemented, so if the Scala 3.0 spec does not include them, they might be still made available under a language flag.

**Migration cost: none**

Being new features, existing code migrates without changes. To be sure, sometimes it would be attractive to rewrite code to make use of the new features in order to increase clarity and conciseness.

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

**Status: not yet settled**

We know we need a practical replacement for current macros. The features listed above are very promising in that respect, but we need more complete implementations and more use cases to reach a final verdict.

**Migration cost: very high**

Existing macro libraries will have to be rewritten from the ground up. In many cases the rewritten libraries will turn out to be simpler and more robust than the old ones, but that does not relieve one of the cost of the rewrites. It's currently unclear to what degree users of macro libraries will be affected. We aim to provide sufficient functionality so that core macros can be re-implemented fully, but given the vast feature set of the various macro extensions to Scala 2 it is difficult to arrive at a workable limitation of scope.

## Changes to Type Checking and Inference

The Scala 3 compiler uses a new algorithm for type inference, which relies on
a general subtype constraint solver. The new algorithm often
[works better than the old](https://contributors.scala-lang.org/t/better-type-inference-for-scala-send-us-your-problematic-cases/2410), but there are inevitably situations where the results of both algorithms differ, leading to errors diagnosed by Scala 3 for programs that the Scala 2 compiler accepts.

**Status: essential**

The new type-checking and inference algorithms are the essential core of the new compiler. They cannot be reverted without dropping the whole implementation of Scala 3.

**Migration cost: high**

Some existing programs will break and, given the complex nature of type inference, it will not always be clear what change caused the breakage and how to fix it.

In our experience, macros and changes in type and implicit argument inference together cause the large majority of problems encountered when porting existing code to Scala 3. The latter source of problems could be addressed systematically by a tool that added all inferred types and implicit arguments to a Scala 2 source code file. Most likely such a tool would be implemented as a Scala 2 compiler plugin. The resulting code would have a greatly increased likelihood to compile under Scala 3, but would often be bulky to the point of being unreadable. A second part of the rewriting tool should then selectively and iteratively remove type and implicit annotations that were synthesized by the first part as long as they compile under Scala 3. This second part could be implemented as a program that invokes the Scala 3 compiler `dotc` programmatically.

Several people have proposed such a tool for some time now. I believe it is time we find the will and the resources to actually implement it.