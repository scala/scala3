---
layout: doc-page
title: "Overview"
---

This section gives an overview of the most important language additions in Scala 3.
The new features all address one or more of four major concerns:

 - [Consistency](consistency)
 - [Safety](safety)
 - [Ergonomics](ergonomics)
 - [Performance](performance)

Scala 3 also drops a number of features that were used rarely, or where experience showed
that they tended to cause problems in codebases. These are [listed in a separate section](./dropped).

Not included in this overview are changes to meta programming and generic programming. So far these have relied on a macro system that had experimental status (`scala.reflect` macros. This macro system will be replaced with a different solution. The current state of the design will be described elsewhere.

<a name="consistency"></a>
## Consistency

Constructs that make the language more consistent internally, and consistent with its foundations. Constructs that admit useful laws of reasoning.

 - Intersection type `A & B`

   Replaces compound type `A with B` (the old syntax is kept for the moment but will
   be deprecated in the future). Intersection types are one of the core features of DOT. They
   are commutative: `A & B` and `B & A` represent the same type.

 - Implicit function type `implicit A => B`.

   Methods and lambdas can have implicit parameters, so it's natural to extend the
   same property to function types. Implicit function types help ergonomics nnd performance
   as well. They can replace many uses of monads at an order of magnitude improvement
   in runtime speed, and with far better composability.

 - Dependent function type `(x: T) => x.S`.

   The result type of a method can refer to its parmeters. We now extend the same capability
   to the result type of a function.

 - Type lambda `[X] => C[X]`

   Type lambdas were encoded previously in an extremely roundabout way, exploiting
   loopholes in Scala's type system which made it Turing complete and unsound. With
   the removal of unrestricted type projection the loopholes are eliminated, so the
   previous encodings are no longer expressible. Type lambdas in the language provide
   a safe andmore ergonimic alternative.

<a name="safety"></a>
## Safety

Constructs that help precise, typecheched domain modeling and that improve the
reliability of refactorings.

 - Union types  `A | B`

   Union types gives fine-grained control over the possible values of a type.
   A union type `A | B` states that a value can be an `A` or a `B` without having
   to widen to a common supertype of `A` and `B`. Union types thus enable more
   precise domain modeling. They are also very useful for interoperating with
   Javascript libraries and JSON protocols.

 - Multiversal Equality

   Multiversal equality is an opt in way to check that comparisons using `==` and
   `!=` only apply to compatible types. It thus closes the biggest remaining hurdle
   to type-based refactoring. Normally, one would wish that one could change the type
   of some value or operation in a large codebase, fix all type errors, and obtain
   at the end a working program. But universal equality `==` works for all types.
   So what should conceptually be a type error would not be reported and
   runtime behavior would change instead. Multiversal equality closes that loophole.

 - Null safety

   (Planned) Adding a `null` value to every type has been called a "Billion Dollar Mistake"
   by its creator, Tony Hoare. With the introduction of union types, we can now do better.
   A type like `String` would not carry the `null` value. To express that a value could
   be `null`, one would use the union type `String | Null`. For backwards compatibility
   and Java interoperability, selecting on a value that's possibly `null` would still be permitted
   but would have a declared effect that a `NullPointerException` can be thrown (see next section).

 - Effect Capabilities

   (Planned) Scala so far is an impure functional programming language in that side effects
   are not tracked. We want to put in the hooks to allow to change this over time. The idea
   is to treat effects as capabilities represented as implicit parameters. Some effect types
   will be defined by the language, others can be added by libraries. Initially, the language
   will likely only cover exceptions as effect capabilities, but it can be extended later
   to mutations and possibly other effects. To ensure backwards compatibility, all effect
   capabilities are initially available in `Predef`. Unimporting effect capabilities from
   `Predef` will enable stricter effect checking, and provide stronger guarantees of purity.

<a name="ergonomics"></a>
## Ergonomics

Constructs that make common programming patterns more concise and readable.

 - Enums `enum Color { case Red, Green, Blue }`

   Enums give a simple way to express a type with a finite set of named values. They
   are found in most languages. The previous encodings of enums in Scala were all had
   problems that prevented universal adoption. The new native `enum` construct in Scala
   is quite flexile; among others it gives a more concise way to write algebraic data types,
   which would otherwise be expressed by a sealed base trait with case classes as alternatives.
   Scala enums will interop with en the host platform. They support multiversal equality
   out of the box, i.e. an enum can only be compared to values of the same enum type.

 - Extension clauses `extension StringOps for String { ... }`

   (Pending) Extension clauses allow to define extension methods and late implementations
   of traits via instance declarations. They are more readable and convey intent better
   than the previous encodings of these features through implicit classes and value classes.
   Extensions will replace implicit classes. Extensions and opaque types together can
   replace almost all usages of value classes.

<a name="performance"></a>
## Performance

 - Opaque Type Aliases `opaque type A = T`

   (Pending) An opaque alias allows to define a new type `A` in terms of an existing type `T`.  Unlike the previous modeling using value classes, opqaue types guarantee no boxing.
   Opaque types are described in detail in [SIP 35](https://docs.scala-lang.org/sips/opaque-types.html).

 - Erased parameters

   Parameters of methods and functions can be declared `erased`. This means that
   the corresponding arguments are only used for type checking purposes and no code
   will be generated for them. Typical candidates for erased parameters are type
   constraints impressed through implicits such as `=:=` and `<:<`. Erased parameters
   help both runtime (since no argument has to be constructed) and compile time
   (since potentially large arguments can be eliminated early).


