---
layout: doc-page
title: "Overview"
---

This section gives an overview of the most important language additions in Scala 3.
The new features address four major concerns:

 - [Consistency](http://dotty.epfl.ch/docs/reference/overview.html#consistency) - improve orthogonality and eliminate restrictions.
 - [Safety](http://dotty.epfl.ch/docs/reference/overview.html#safety) - enable precise domain modeling and safe refactoring.
 - [Ergonomics](http://dotty.epfl.ch/docs/reference/overview.html#ergonomics) - support readable and concise code.
 - [Performance](http://dotty.epfl.ch/docs/reference/overview.html#performance) - remove performance penalties for high-level code.

Scala 3 also drops a number of features that were used rarely, or where experience showed
that they tended to cause problems. These are listed separately in the [Dropped Features](http://dotty.epfl.ch/docs) section.

Another important set of changes is about meta programming and generative programming. So far these have relied on a [macro system](https://docs.scala-lang.org/overviews/macros/overview.html) that had experimental status. This macro system will be replaced with a different solution that extends [principled meta programming](http://dotty.epfl.ch/docs/reference/other-new-features/principled-meta-programming.html) and [inline](http://dotty.epfl.ch/docs/reference/other-new-features/inline.html) definitions with some reflective capabilities. The current state of the full design and its ramifications for generative programming will be described elsewhere.

<a name="consistency"></a>
## Consistency

The primary goal of the language constructs in this section is to make the language more consistent, both internally, and in relationship to its [foundations](http://www.scala-lang.org/blog/2016/02/03/essence-of-scala.html).

 - [Intersection types](http://dotty.epfl.ch/docs/reference/new-types/intersection-types.html) `A & B`

   They replace compound types `A with B` (the old syntax is kept for the moment but will
   be deprecated in the future). Intersection types are one of the core features of DOT. They
   are commutative: `A & B` and `B & A` represent the same type.

 - [Context query types](http://dotty.epfl.ch/docs/reference/contextual/query-types.html) `given A => B`.

   Methods and lambdas can have implicit parameters, so it's natural to extend the
   same property to function types. context query types help ergonomics and performance
   as well. They can replace many uses of monads, offering better composability and an order of magnitude improvement in runtime speed.

 - [Dependent function types](http://dotty.epfl.ch/docs/reference/new-types/dependent-function-types.html) `(x: T) => x.S`.

   The result type of a method can refer to its parameters. We now extend the same capability
   to the result type of a function.

 - [Trait parameters](http://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html) `trait T(x: S)`

   Traits can now have value parameters, just like classes do. This replaces the more complex [early initializer](http://dotty.epfl.ch/docs/reference/dropped-features/early-initializers.html) syntax.

 - Generic tuples

   ([Pending](https://github.com/lampepfl/dotty/pull/2199)) Tuples with arbitrary numbers of elements are treated as sequences of nested pairs. E.g. `(a, b, c)` is shorthand for `(a, (b, (c, ())))`. This lets us drop the current limit of 22 for maximal tuple length and it allows generic programs over tuples analogous to what is currently done for `HList`.

<a name="safety"></a>
## Safety

Listed in this section are new language constructs that help precise, typechecked domain modeling and that improve the reliability of refactorings.

 - [Union types](http://dotty.epfl.ch/docs/reference/new-types/union-types.html)  `A | B`

   Union types gives fine-grained control over the possible values of a type.
   A union type `A | B` states that a value can be an `A` or a `B` without having
   to widen to a common supertype of `A` and `B`. Union types thus enable more
   precise domain modeling. They are also very useful for interoperating with
   Javascript libraries and JSON protocols.

 - [Multiversal Equality](http://dotty.epfl.ch/docs/reference/contextual/multiversal-equality.html)

   Multiversal equality is an opt-in way to check that comparisons using `==` and
   `!=` only apply to compatible types. It thus removes the biggest remaining hurdle
   to type-based refactoring. Normally, one would wish that one could change the type
   of some value or operation in a large code base, fix all type errors, and obtain
   at the end a working program. But universal equality `==` works for all types.
   So what should conceptually be a type error would not be reported and
   runtime behavior might change instead. Multiversal equality closes that loophole.

 - Restrict Implicit Conversions

   ([Pending](https://github.com/lampepfl/dotty/pull/4229))
   Implicit conversions are very easily mis-used, which makes them the cause of much surprising behavior.
   We now require a language feature import not only when an implicit conversion is defined
   but also when it is applied. This protects users of libraries that define implicit conversions
   from being bitten by unanticipated feature interactions.

 - Null safety

   (Planned) Adding a `null` value to every type has been called a "Billion Dollar Mistake"
   by its inventor, Tony Hoare. With the introduction of union types, we can now do better.
   A type like `String` will not carry the `null` value. To express that a value can
   be `null`, one will use the union type `String | Null` instead. For backwards compatibility and Java interoperability, selecting on a value that's possibly `null` will still be permitted but will have a declared effect that a `NullPointerException` can be thrown (see next section).

 - Effect Capabilities

   (Planned) Scala so far is an impure functional programming language in that side effects
   are not tracked. We want to put in the hooks to allow to change this over time. The idea
   is to treat effects as capabilities represented as implicit parameters. Some effect types
   will be defined by the language, others can be added by libraries. Initially, the language
   will likely only cover exceptions as effect capabilities, but this can be extended later
   to mutations and other effects. To ensure backwards compatibility, all effect
   capabilities are initially available in `Predef`. Un-importing effect capabilities from
   `Predef` will enable stricter effect checking, and provide stronger guarantees of purity.

<a name="ergonomics"></a>
## Ergonomics

The primary goal of the language constructs in this section is to make common programming patterns more concise and readable.

 - [Enums](http://dotty.epfl.ch/docs/reference/enums/enums.html) `enum Color { case Red, Green, Blue }`

   Enums give a simple way to express a type with a finite set of named values. They
   are found in most languages. The previous encodings of enums as library-defined types
   were not fully satisfactory and consequently were not adopted widely. The new native `enum` construct in Scala is quite flexible; among others it gives a more concise way to express [algebraic data types](http://dotty.epfl.ch/docs/reference/enums/adts.html).
   Scala enums will interoperate with  the host platform. They support multiversal equality
   out of the box, i.e. an enum can only be compared to values of the same enum type.

 - [Type lambdas](http://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html) `[X] => C[X]`

   Type lambdas were encoded previously in a roundabout way, exploiting
   loopholes in Scala's type system which made it Turing complete. With
   the removal of [unrestricted type projection](dropped-features/type-projection.html), the loopholes are eliminated, so the
   previous encodings are no longer expressible. Type lambdas in the language provide
   a safe and more ergonomic alternative.

 - Extension clauses `extension StringOps for String { ... }`

   ([Pending](https://github.com/lampepfl/dotty/pull/4114)) Extension clauses allow to define extension methods and late implementations
   of traits via instance declarations. They are more readable and convey intent better
   than the previous encodings of these features through implicit classes and value classes.
   Extensions will replace implicit classes. Extensions and opaque types together can
   replace almost all usages of value classes. Value classes are kept around for the
   time being since there might be a new good use case for them in the future if the host platform supports "structs" or some other way to express multi-field value classes.

<a name="performance"></a>
## Performance

The primary goal of the language constructs in this section is to enable high-level, safe code without having to pay a performance penalty.

 - [Opaque Type Aliases](https://dotty.epfl.ch/docs/reference/other-new-features/opaques.html) `opaque type A = T`

   An opaque alias defines a new type `A` in terms of an existing type `T`.  Unlike the previous modeling using value classes, opaque types never box. Opaque types are described in detail in [SIP 35](https://docs.scala-lang.org/sips/opaque-types.html).

 - [Erased parameters](http://dotty.epfl.ch/docs/reference/other-new-features/erased-terms.html)

   Parameters of methods and functions can be declared `erased`. This means that
   the corresponding arguments are only used for type checking purposes and no code
   will be generated for them. Typical candidates for erased parameters are type
   constraints such as `=:=` and `<:<` that are expressed through implicits.
   Erased parameters improve both run times (since no argument has to be constructed) and compile times (since potentially large arguments can be eliminated early).

See also: [A classification of proposed language features](./features-classification.html)
